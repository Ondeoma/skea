package skea.api

import cats.effect.*
import cats.syntax.all.*
import fs2.io.file.Path as FPath
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.headers.{`Content-Disposition`, `Content-Type`}
import org.typelevel.ci.CIString
import skea.db.*
import skea.shared.*
import sbirds.storage.BlobStore

import java.security.MessageDigest
import java.util.UUID

/** 添付ファイルの保存と配信（タスクの画面とヘルプデスクの窓口で共通）。権限の確認は呼ぶ側で行う。 */
final class Attachments(tasks: TaskStore, users: UserStore, hub: EventHub, blobs: Option[BlobStore]) {

  def available: Boolean = blobs.isDefined

  /** 受け取ったファイルを key で S3 に置き、SHA-256 を返す（保存先がなければ None）。 */
  def put(key: String, up: Upload): IO[Option[String]] =
    blobs.traverse(store => Attachments.sha256Hex(up.path).flatTap(_ => store.put(key, up.path, up.contentType)))

  /** S3 のファイルを消す（失敗しても止めない）。 */
  def remove(keys: List[String]): IO[Unit] =
    blobs.traverse_(store => keys.traverse_(k => store.delete(k).handleError(_ => ())))

  /** 受け取ったファイルを S3 に置き、タスクに添付する。 */
  def save(op: Op, t: TaskCore, up: Upload): IO[Option[AttachmentDto]] = {
    val key = s"tasks/${t.id}/${UUID.randomUUID()}"
    put(key, up).flatMap(_.traverse { sha =>
      for {
        id <- tasks.addAttachment(t.id, key, up.name, up.size, up.contentType, sha, op.userId)
        _ <- hub.emit(
          op,
          t.projectId,
          Some(t.id),
          EventKind.AttachmentAdded,
          Json.obj("attachmentId" -> id.asJson, "fileName" -> up.name.asJson)
        )
        dto <- dto(op.userId, id, up, s"/api/attachments/$id")
      } yield dto
    })
  }

  /** 添付ファイルの表示用の情報（今アップロードしたもの）。 */
  def dto(userId: UUID, id: UUID, up: Upload, url: String): IO[AttachmentDto] =
    users.find(userId).map(me =>
      AttachmentDto(
        id,
        up.name,
        up.size,
        up.contentType,
        me.map(u => UserBrief(u.id, u.name, u.avatarUrl)),
        java.time.Instant.now(),
        url
      )
    )

  /** ファイルの中身を返す（画像・PDF などはブラウザで開き、それ以外はダウンロードにする）。 */
  def download(key: String, name: String, ct: String): IO[Response[IO]] = {
    def status(st: Status, msg: String) =
      IO.pure(Response[IO](st).withEntity(ErrorDto(msg))(using org.http4s.circe.CirceEntityCodec.circeEntityEncoder))
    blobs.fold(status(Status.ServiceUnavailable, "保存先が設定されていません"))(store =>
      store.get(key).flatMap {
        case None => status(Status.NotFound, "ファイルの中身が見つかりません")
        case Some((meta, bytes)) =>
          val inline = Attachments.inlineTypes.contains(ct)
          val mt = if inline then MediaType.parse(ct).getOrElse(MediaType.application.`octet-stream`)
          else MediaType.application.`octet-stream`
          val hs = List[Header.ToRaw](
            `Content-Type`(mt),
            `Content-Disposition`(if inline then "inline" else "attachment", Map(CIString("filename*") -> name)),
            "X-Content-Type-Options" -> "nosniff",
            "Cache-Control" -> "private, max-age=3600"
          ) ++ (if ct == "application/pdf" then Nil else List[Header.ToRaw]("Content-Security-Policy" -> "sandbox"))
          IO.pure(Response[IO](Status.Ok).withEntity(Entity.stream(bytes, Some(meta.size))).putHeaders(hs*))
      }
    )
  }
}

object Attachments {
  val inlineTypes = Set(
    "image/png",
    "image/jpeg",
    "image/gif",
    "image/webp",
    "application/pdf",
    "text/plain",
    "video/mp4",
    "video/webm",
    "audio/mpeg",
    "audio/ogg",
    "audio/wav"
  )

  def sha256Hex(p: FPath): IO[String] = IO.blocking {
    val md = MessageDigest.getInstance("SHA-256")
    val in = java.nio.file.Files.newInputStream(p.toNioPath)
    try {
      val buf = new Array[Byte](64 * 1024)
      var n = in.read(buf)
      while n > 0 do { md.update(buf, 0, n); n = in.read(buf) }
    } finally in.close()
    md.digest().map(b => f"${b & 0xff}%02x").mkString
  }
}
