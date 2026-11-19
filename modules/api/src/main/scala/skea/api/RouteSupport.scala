package skea.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Decoder
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import skea.core.*
import skea.db.*
import sbirds.http.Errors
import sbirds.shared.AppError

import java.util.UUID

trait RouteSupport extends Http4sDsl[IO] {
  protected def sessions: SessionAuth
  protected def access: Access2

  /** エラーの応答（コード付きでログに出る）. detail を省くとコードの既定の文言. */
  protected def err(
    code: ErrorCode,
    detail: String | Null = null
  ): IO[Response[IO]] = Errors.respond(AppError(code, Option(detail)))

  protected def fail(
    e: AppError
  ): IO[Response[IO]] = Errors.respond(e)

  /** 結果が Right なら f、Left なら失敗の応答。 */
  protected def respond[A](
    r: Res[A]
  )(
    f: A => IO[Response[IO]]
  ): IO[Response[IO]] = r.flatMap(_.fold(fail, f))

  protected def okJson[A: io.circe.Encoder](
    r: Res[A]
  ): IO[Response[IO]] = respond(r)(a => Ok(a))

  /** POST だが読み取りだけの API（本文で条件を送るもの）。 */
  private val readOnlyPosts = Set("/api/tasks/lookup")
  private def readOnlyPost(
    req: Request[IO]
  ): Boolean = req.method == Method.POST && readOnlyPosts.contains(req.pathInfo.renderString)

  private val safeMethods = Set(Method.GET, Method.HEAD, Method.OPTIONS)

  /**
   * Cookie のセッションでの更新系の要求が、別のサイトから送られたものでないか（CSRF 対策）。
   * ブラウザが付ける Origin（なければ Referer）が PUBLIC_URL と同じ origin であることを確かめる。開発中（DEV_LOGIN）は localhost も許す。
   */
  private def sameOrigin(
    req: Request[IO]
  ): Boolean = {
    def origin(
      u: String
    ): Option[String] =
      Uri.fromString(u).toOption.flatMap(x =>
        (x.scheme, x.authority).mapN(
          (
            sc,
            a
          ) => s"${sc.value.toString.toLowerCase}://${a.host.value.toLowerCase}${a.port.fold("")(":" + _)}"
        )
      )
    val mine = origin(sessions.publicUrl)
    def ok(
      o: String
    ): Boolean = origin(o).exists(x =>
      mine.contains(x) ||
        (sessions.devLogin &&
          Uri.fromString(o).toOption.flatMap(_.host).exists(h => Set("localhost", "127.0.0.1").contains(h.value)))
    )
    req.headers.get(org.typelevel.ci.CIString("Origin")).map(_.head.value).filter(_ != "null") match {
      case Some(o) => ok(o)
      case None => req.headers.get(org.typelevel.ci.CIString("Referer")).map(_.head.value) match {
          case Some(r) => ok(r)
          // Origin も Referer もない: ブラウザ以外（curl など）。ブラウザが別サイトからと示していれば拒否する
          case None => !req.headers.get(org.typelevel.ci.CIString("Sec-Fetch-Site")).exists(h =>
              h.head.value == "cross-site" || h.head.value == "same-site"
            )
        }
    }
  }

  /** ログインしていなければ 401。更新系の要求を read スコープのキーで送ったら 403。 */
  protected def authed(
    req: Request[IO]
  )(
    f: AuthSession => IO[Response[IO]]
  ): IO[Response[IO]] =
    sessions.fromRequest(req).flatMap {
      case None => err(ErrorCode.NotLoggedIn)
      case Some(s) =>
        // 操作したユーザーを、トレース（スパンの属性）とこの中のログに載せる
        sbirds.observe.Telemetry.withUser(s.user.id.toString, s.user.name) {
          if s.isSession && !safeMethods.contains(req.method) && !sameOrigin(req) then
            err(ErrorCode.CrossSiteRequest)
          else if !s.canWrite && !safeMethods.contains(req.method) && !readOnlyPost(req) then
            err(ErrorCode.ApiKeyNeedsWrite)
          else f(s)
        }
    }

  /** システム管理者のみ（APIキーは admin スコープが必要）。 */
  protected def admin(
    req: Request[IO]
  )(
    f: AuthSession => IO[Response[IO]]
  ): IO[Response[IO]] =
    authed(req)(s => if s.canAdmin then f(s) else err(ErrorCode.AdminOnly))

  /** Cookie のセッションのみ（APIキーの発行など）。 */
  protected def sessionOnly(
    req: Request[IO]
  )(
    f: AuthSession => IO[Response[IO]]
  ): IO[Response[IO]] =
    authed(req)(s => if s.isSession then f(s) else err(ErrorCode.SessionOnly))

  protected def body[A: Decoder](
    req: Request[IO]
  )(
    f: A => IO[Response[IO]]
  ): IO[Response[IO]] =
    req.as[A].attempt.flatMap {
      case Left(e) => err(ErrorCode.RequestBodyInvalid, s"リクエストの内容が正しくありません: ${e.getMessage.take(200)}")
      case Right(b) => f(b)
    }

  /** プロジェクトの権限を確認してから処理する。 */
  protected def withProject(
    req: Request[IO],
    pid: UUID,
    need: Role
  )(
    f: (AuthSession, Role) => IO[Response[IO]]
  ): IO[Response[IO]] =
    authed(req)(s => respond(access.project(s, pid, need))(r => f(s, r)))

  /** タスクの権限を確認してから処理する。 */
  protected def withTask(
    req: Request[IO],
    id: String,
    need: Role
  )(
    f: (AuthSession, TaskCore, Role) => IO[Response[IO]]
  ): IO[Response[IO]] =
    authed(req)(s =>
      respond(access.task(s, id, need))(
        (
          t,
          r
        ) => f(s, t, r)
      )
    )

  /** multipart/form-data の file を一時ファイルに受け取る（上限を超えたら 413）。 */
  protected def withUpload(
    req: Request[IO],
    maxBytes: Long
  )(
    f: Upload => IO[Response[IO]]
  ): IO[Response[IO]] =
    req.as[org.http4s.multipart.Multipart[IO]].attempt.flatMap {
      case Left(_) => err(ErrorCode.UploadNotMultipart)
      case Right(mp) =>
        mp.parts.find(_.name.contains("file")) match {
          case None => err(ErrorCode.UploadFileMissing)
          case Some(part) =>
            val name =
              part.filename.map(_.trim).filter(_.nonEmpty).getOrElse("file").replaceAll("[\\\\/\\r\\n]", "_").take(200)
            val ct = part.contentType.map(c => s"${c.mediaType.mainType}/${c.mediaType.subType}").getOrElse(
              "application/octet-stream"
            )
            fs2.io.file.Files[IO].tempFile.use { tmp =>
              part.body.take(maxBytes + 1).through(fs2.io.file.Files[IO].writeAll(tmp)).compile.drain *>
                fs2.io.file.Files[IO].size(tmp).flatMap { size =>
                  if size > maxBytes then
                    err(ErrorCode.UploadTooLarge, s"ファイルが大きすぎます（上限 ${math.max(1, maxBytes / 1024 / 1024)}MB）")
                  else f(Upload(name, ct, tmp, size))
                }
            }
        }
    }

  protected object UuidVar {
    def unapply(
      s: String
    ): Option[UUID] = uuidOpt(s)
  }

  protected def param(
    req: Request[IO],
    k: String
  ): Option[String] = req.params.get(k).map(_.trim).filter(_.nonEmpty)

  protected def intParam(
    req: Request[IO],
    k: String,
    default: Int,
    max: Int
  ): Int =
    param(req, k).flatMap(_.toIntOption).map(v => math.max(1, math.min(v, max))).getOrElse(default)
}
