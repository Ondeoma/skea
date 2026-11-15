package skea.api

import cats.effect.*
import cats.syntax.all.*
import fs2.io.file.Path as FPath
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.{`Content-Disposition`, `Content-Type`}
import org.typelevel.ci.CIString
import skea.core.*
import skea.db.*
import skea.shared.*
import skea.storage.BlobStore

import java.security.MessageDigest
import java.util.UUID

/** タスクと、それに付くもの。 */
final class TaskRoutes(
    tasks: TaskStore,
    users: UserStore,
    activity: ActivityStore,
    integ: IntegrationStore,
    svc: TaskService,
    hub: EventHub,
    automation: AutomationEngine,
    blobs: Option[BlobStore],
    emoji: EmojiStore,
    val sessions: SessionAuth,
    val access: Access2,
    cfg: ApiConfig
) extends RouteSupport:

  private def detail(s: AuthSession, t: TaskCore, role: Role): IO[Response[IO]] =
    tasks.detail(t.id).flatMap {
      case None => err(Status.NotFound, "タスクが見つかりません")
      case Some(d) =>
        activity.markTaskRead(s.user.id, t.id).whenA(s.isSession) *>
          Ok(TaskDetailDto(d.task, d.core.description, d.core.description.fold("")(RichText.toHtml), d.ancestors, d.subtasks, d.watchers,
            d.watchers.exists(_.id == s.user.id), d.links, d.predecessors, d.successors, d.fields, d.commits, d.attachments, role.asString,
            d.latestEventId))
    }

  /** リアクションを付ける・外す（決まった絵文字だけ。閲覧できる人なら付けられる）。 */
  private def reaction(req: Request[IO], cid: UUID, on: Boolean): IO[Response[IO]] =
    authed(req)(s => tasks.commentAuthor(cid).flatMap {
      case None => err(Status.NotFound, "コメントが見つかりません")
      case Some((tid, _)) => respond(access.task(s, tid.toString, Role.View))((t, _) =>
        val emojiName = if on then req.as[ReactionReq].attempt.map(_.toOption.map(_.emoji)) else IO.pure(param(req, "emoji"))
        // 決まった絵文字か、登録済みのカスタム絵文字（:名前:）だけ。外すときは何でも外せる
        val usable = emojiName.flatMap {
          case Some(e) if !on || Reactions.all.contains(e) => IO.pure(Some(e))
          case Some(e) => Reactions.customName(e).fold(IO.pure(None: Option[String]))(n => emoji.find(n).map(_.map(_ => e)))
          case None    => IO.pure(None)
        }
        usable.flatMap {
          case Some(e) if s.canWrite =>
            tasks.react(cid, s.user.id, e, on) *>
              tasks.event(t.projectId, Some(t.id), Some(s.user.id), s.viaString, "comment.reacted", Json.obj("commentId" -> cid.asJson)) *>
              tasks.comment(cid).flatMap(c => Ok(c.map(_.reactions).getOrElse(Nil)))
          case Some(_) => err(Status.Forbidden, "このAPIキーは参照専用です")
          case None    => err(Status.BadRequest, "使えない絵文字です（決まった絵文字か、登録したカスタム絵文字 :名前: を指定してください）")
        })
    })

  private val inlineTypes = Set("image/png", "image/jpeg", "image/gif", "image/webp", "application/pdf", "text/plain", "video/mp4",
    "video/webm", "audio/mpeg", "audio/ogg", "audio/wav")

  private def sha256Hex(p: FPath): IO[String] = IO.blocking {
    val md = MessageDigest.getInstance("SHA-256")
    val in = java.nio.file.Files.newInputStream(p.toNioPath)
    try
      val buf = new Array[Byte](64 * 1024)
      var n   = in.read(buf)
      while n > 0 do { md.update(buf, 0, n); n = in.read(buf) }
    finally in.close()
    md.digest().map(b => f"${b & 0xff}%02x").mkString
  }

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "tasks" / id =>
      withTask(req, id, Role.View)(detail)

    case req @ PATCH -> Root / "api" / "tasks" / id =>
      withTask(req, id, Role.Edit)((s, t, _) => body[Json](req)(j => svc.parsePatch(j) match
        case Left(f)   => fail(f)
        case Right(ch) => respond(svc.update(Op(s), t, ch))(Ok(_))))

    case req @ DELETE -> Root / "api" / "tasks" / id =>
      withTask(req, id, Role.Edit)((s, t, _) => respond(svc.delete(Op(s), t))(_ => NoContent()))

    case req @ POST -> Root / "api" / "tasks" / id / "move" =>
      withTask(req, id, Role.Edit)((s, t, _) => body[MoveTaskReq](req)(r => respond(svc.move(Op(s), t, r))(Ok(_))))

    case req @ POST -> Root / "api" / "tasks" / "bulk" =>
      authed(req)(s => body[BulkUpdateReq](req)(r =>
        if r.taskIds.size > 500 then err(Status.BadRequest, "一度に更新できるのは500件までです")
        else respond(svc.bulk(Op(s), r.taskIds, r.patch))(n => Ok(Json.obj("updated" -> n.asJson)))))

    case req @ GET -> Root / "api" / "tasks" / id / "history" =>
      withTask(req, id, Role.View)((_, t, _) => tasks.history(t.id, intParam(req, "limit", 100, 500)).flatMap(Ok(_)))

    // ---------------------------------------------------------------- ウォッチャー
    case req @ POST -> Root / "api" / "tasks" / id / "watch" =>
      withTask(req, id, Role.View)((s, t, _) => tasks.watch(t.id, s.user.id) *> NoContent())

    case req @ DELETE -> Root / "api" / "tasks" / id / "watch" =>
      withTask(req, id, Role.View)((s, t, _) => tasks.unwatch(t.id, s.user.id) *> NoContent())

    case req @ POST -> Root / "api" / "tasks" / id / "watchers" =>
      withTask(req, id, Role.Edit)((_, t, _) => body[Json](req)(j => j.hcursor.get[UUID]("userId").toOption match
        case None    => err(Status.BadRequest, "userId を指定してください")
        case Some(u) => users.find(u).flatMap(_.fold(err(Status.NotFound, "ユーザーが見つかりません"))(_ => tasks.watch(t.id, u) *> NoContent()))))

    case req @ DELETE -> Root / "api" / "tasks" / id / "watchers" / UuidVar(uid) =>
      withTask(req, id, Role.Edit)((_, t, _) => tasks.unwatch(t.id, uid) *> NoContent())

    // ---------------------------------------------------------------- リンク
    case req @ GET -> Root / "api" / "tasks" / id / "links" =>
      withTask(req, id, Role.View)((_, t, _) => tasks.links(t.id).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "tasks" / id / "links" =>
      withTask(req, id, Role.Edit)((s, t, _) => body[CreateLinkReq](req) { r =>
        (r.targetKey.map(_.trim).filter(_.nonEmpty), r.url.map(_.trim).filter(_.nonEmpty)) match
          case (Some(k), _) =>
            if !LinkKind.taskKinds.contains(r.kind) then err(Status.BadRequest, s"リンクの種類は ${LinkKind.taskKinds.mkString(" / ")} です")
            else respond(access.task(s, k, Role.View)) { (target, _) =>
              if target.id == t.id then err(Status.BadRequest, "自分自身にはリンクできません")
              else tasks.addLink(t.id, r.kind, Some(target.id), None, "", s.user.id).flatMap(lid =>
                hub.touched(Op(s), t, "links", "link" -> Json.obj("kind" -> r.kind.asJson, "key" -> target.key.asJson)) *>
                  Created(Json.obj("id" -> lid.asJson)))
            }
          case (None, Some(u)) =>
            RichText.safeUrl(u).filter(x => x.startsWith("http")) match
              case None => err(Status.BadRequest, "URL は http(s):// で始めてください")
              case Some(url) =>
                tasks.addLink(t.id, LinkKind.Web, None, Some(url), r.title.map(_.trim).filter(_.nonEmpty).getOrElse(url), s.user.id).flatMap(lid =>
                  hub.touched(Op(s), t, "links") *>
                    Created(Json.obj("id" -> lid.asJson)))
          case _ => err(Status.BadRequest, "targetKey か url を指定してください")
      })

    case req @ DELETE -> Root / "api" / "tasks" / id / "links" / UuidVar(lid) =>
      withTask(req, id, Role.Edit)((s, t, _) => tasks.deleteLink(t.id, lid).flatMap {
        case true  => hub.touched(Op(s), t, "links") *> NoContent()
        case false => err(Status.NotFound, "リンクが見つかりません")
      })

    // ---------------------------------------------------------------- 依存関係（このタスクの先行タスク）
    case req @ POST -> Root / "api" / "tasks" / id / "dependencies" =>
      withTask(req, id, Role.Edit)((s, t, _) => body[CreateDependencyReq](req)(r =>
        respond(access.task(s, r.predecessorKey, Role.View)) { (pred, _) =>
          val kind = r.kind.filter(Set("fs", "ss", "ff", "sf")).getOrElse("fs")
          if pred.projectId != t.projectId then err(Status.BadRequest, "依存関係は同じプロジェクトのタスクだけに設定できます")
          else tasks.addDependency(pred.id, t.id, kind).orFail.flatMap(_.fold(fail, _ =>
            hub.touched(Op(s), t, "dependencies", "predecessor" -> pred.key.asJson) *> NoContent()))
        }))

    case req @ DELETE -> Root / "api" / "tasks" / id / "dependencies" / UuidVar(pred) =>
      withTask(req, id, Role.Edit)((s, t, _) => tasks.deleteDependency(pred, t.id).flatMap {
        case true  => hub.touched(Op(s), t, "dependencies") *> NoContent()
        case false => err(Status.NotFound, "依存関係が見つかりません")
      })

    // ---------------------------------------------------------------- 作業時間
    case req @ GET -> Root / "api" / "tasks" / id / "worklogs" =>
      withTask(req, id, Role.View)((_, t, _) => tasks.worklogs(t.id).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "tasks" / id / "worklogs" =>
      withTask(req, id, Role.Edit)((s, t, _) => body[WorklogReq](req)(r => respond(svc.addWorklog(Op(s), t, r))(Created(_))))

    case req @ PUT -> Root / "api" / "worklogs" / UuidVar(wid) =>
      authed(req)(s => tasks.worklog(wid).flatMap {
        case None => err(Status.NotFound, "作業時間が見つかりません")
        case Some(w) =>
          respond(access.project(s, w.projectId, Role.Edit)) { role =>
            if w.user.id != s.user.id && role < Role.Admin then err(Status.Forbidden, "自分の作業時間だけ編集できます")
            else body[WorklogReq](req)(r => Dates.parse(Some(r.workDate), "作業日") match
              case Right(Some(d)) if r.minutes > 0 && r.minutes <= 24 * 60 =>
                tasks.updateWorklog(wid, d, r.minutes, r.note.getOrElse("").trim) *>
                  hub.emit(Op(s), w.projectId, Some(w.taskId), "worklog.updated", Json.obj("worklogId" -> wid.asJson)) *> NoContent()
              case Left(f) => fail(f)
              case _       => err(Status.BadRequest, "作業日と作業時間（1分〜24時間）を確認してください"))
          }
      })

    case req @ DELETE -> Root / "api" / "worklogs" / UuidVar(wid) =>
      authed(req)(s => tasks.worklog(wid).flatMap {
        case None => err(Status.NotFound, "作業時間が見つかりません")
        case Some(w) =>
          respond(access.project(s, w.projectId, Role.Edit)) { role =>
            if w.user.id != s.user.id && role < Role.Admin then err(Status.Forbidden, "自分の作業時間だけ削除できます")
            else tasks.deleteWorklog(wid) *> hub.emit(Op(s), w.projectId, Some(w.taskId), "worklog.deleted", Json.obj("worklogId" -> wid.asJson)) *> NoContent()
          }
      })

    // ---------------------------------------------------------------- コメント
    case req @ GET -> Root / "api" / "tasks" / id / "comments" =>
      withTask(req, id, Role.View)((_, t, _) => tasks.comments(t.id).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "tasks" / id / "comments" =>
      withTask(req, id, Role.Edit)((s, t, _) => body[CommentReq](req)(r =>
        respond(svc.addComment(Op(s), t, r.body))(c => tasks.deleteDraft(s.user.id, t.id) *> Created(c))))

    // ---------------------------------------------------------------- リアクション
    case req @ POST -> Root / "api" / "comments" / UuidVar(cid) / "reactions" => reaction(req, cid, on = true)
    case req @ DELETE -> Root / "api" / "comments" / UuidVar(cid) / "reactions" => reaction(req, cid, on = false)

    // ---------------------------------------------------------------- コメントの下書き（自分の分だけ）
    case req @ GET -> Root / "api" / "tasks" / id / "draft" =>
      withTask(req, id, Role.View)((s, t, _) => tasks.draft(s.user.id, t.id).flatMap(d => Ok(d.asJson)))

    case req @ PUT -> Root / "api" / "tasks" / id / "draft" =>
      withTask(req, id, Role.Edit)((s, t, _) => body[CommentReq](req)(r =>
        if !RichText.valid(r.body) then err(Status.BadRequest, "body は Tiptap の文書（type: doc）です")
        else if r.body.noSpaces.length > 1_000_000 then err(Status.PayloadTooLarge, "下書きが大きすぎます")
        else if RichText.isBlank(r.body) then tasks.deleteDraft(s.user.id, t.id) *> NoContent()
        else tasks.saveDraft(s.user.id, t.id, r.body) *> NoContent()))

    case req @ DELETE -> Root / "api" / "tasks" / id / "draft" =>
      withTask(req, id, Role.View)((s, t, _) => tasks.deleteDraft(s.user.id, t.id) *> NoContent())

    case req @ PUT -> Root / "api" / "comments" / UuidVar(cid) =>
      authed(req)(s => tasks.commentAuthor(cid).flatMap {
        case None => err(Status.NotFound, "コメントが見つかりません")
        case Some((tid, _)) => respond(access.task(s, tid.toString, Role.Edit))((t, _) =>
          body[CommentReq](req)(r => respond(svc.updateComment(Op(s), t, cid, r.body))(Ok(_))))
      })

    case req @ DELETE -> Root / "api" / "comments" / UuidVar(cid) =>
      authed(req)(s => tasks.commentAuthor(cid).flatMap {
        case None => err(Status.NotFound, "コメントが見つかりません")
        case Some((tid, _)) => respond(access.task(s, tid.toString, Role.Edit))((t, role) =>
          respond(svc.deleteComment(Op(s), t, cid, role))(_ => NoContent()))
      })

    // ---------------------------------------------------------------- 添付ファイル
    case req @ GET -> Root / "api" / "tasks" / id / "attachments" =>
      withTask(req, id, Role.View)((_, t, _) => tasks.attachments(t.id).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "tasks" / id / "attachments" =>
      withTask(req, id, Role.Edit)((s, t, _) => blobs match
        case None => err(Status.ServiceUnavailable, "添付ファイルの保存先（S3）が設定されていません")
        case Some(store) =>
          withUpload(req, cfg.maxUploadBytes) { up =>
            val key = s"tasks/${t.id}/${UUID.randomUUID()}"
            for
              sha <- sha256Hex(up.path)
              _   <- store.put(key, up.path, up.contentType)
              id2 <- tasks.addAttachment(t.id, key, up.name, up.size, up.contentType, sha, s.user.id)
              _   <- hub.emit(Op(s), t.projectId, Some(t.id), EventKind.AttachmentAdded, Json.obj("attachmentId" -> id2.asJson, "fileName" -> up.name.asJson))
              me  <- users.find(s.user.id)
              r   <- Created(AttachmentDto(id2, up.name, up.size, up.contentType, me.map(u => UserBrief(u.id, u.name, u.avatarUrl)),
                       java.time.Instant.now(), s"/api/attachments/$id2"))
            yield r
          })

    case req @ GET -> Root / "api" / "attachments" / UuidVar(aid) =>
      authed(req)(s => tasks.attachment(aid).flatMap {
        case None => err(Status.NotFound, "添付ファイルが見つかりません")
        case Some((tid, key, name, ct, _)) =>
          respond(access.task(s, tid.toString, Role.View))((_, _) => blobs.fold(err(Status.ServiceUnavailable, "保存先が設定されていません"))(store =>
            store.get(key).flatMap {
              case None => err(Status.NotFound, "ファイルの中身が見つかりません")
              case Some((meta, bytes)) =>
                val inline = inlineTypes.contains(ct)
                val mt     = if inline then MediaType.parse(ct).getOrElse(MediaType.application.`octet-stream`) else MediaType.application.`octet-stream`
                val hs = List[Header.ToRaw](
                  `Content-Type`(mt),
                  `Content-Disposition`(if inline then "inline" else "attachment", Map(CIString("filename*") -> name)),
                  "X-Content-Type-Options" -> "nosniff",
                  "Cache-Control" -> "private, max-age=3600"
                ) ++ (if ct == "application/pdf" then Nil else List[Header.ToRaw]("Content-Security-Policy" -> "sandbox"))
                IO.pure(Response[IO](Status.Ok).withEntity(Entity.stream(bytes, Some(meta.size))).putHeaders(hs*))
            }))
      })

    case req @ DELETE -> Root / "api" / "attachments" / UuidVar(aid) =>
      authed(req)(s => tasks.attachment(aid).flatMap {
        case None => err(Status.NotFound, "添付ファイルが見つかりません")
        case Some((tid, _, name, _, _)) =>
          respond(access.task(s, tid.toString, Role.Edit))((t, _) =>
            tasks.deleteAttachment(aid).flatMap(k => k.traverse_(key => blobs.traverse_(_.delete(key).handleError(_ => ())))) *>
              hub.touched(Op(s), t, "attachments", "removed" -> name.asJson) *> NoContent())
      })

    // ---------------------------------------------------------------- 手動の自動化
    case req @ GET -> Root / "api" / "tasks" / id / "automations" =>
      withTask(req, id, Role.View)((_, t, _) => integ.rulesFor(t.projectId, "manual").flatMap(rs => Ok(rs.map(r => ManualRuleDto(r.id, r.name)))))

    case req @ POST -> Root / "api" / "tasks" / id / "automations" / UuidVar(rid) / "run" =>
      withTask(req, id, Role.Edit)((s, t, _) => respond(automation.manual(Op(s), rid, t))(_ => Accepted(Json.obj())))

  }
