package skea.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skea.core.*
import skea.db.*
import skea.shared.*
import sbirds.storage.BlobStore

import java.util.UUID

/** タスクと、それに付くもの。 */
final class TaskRoutes(tasks: TaskStore,
                       users: UserStore,
                       activity: ActivityStore,
                       integ: IntegrationStore,
                       svc: TaskService,
                       hub: EventHub,
                       automation: AutomationEngine,
                       blobs: Option[BlobStore],
                       files: Attachments,
                       emoji: EmojiStore,
                       val sessions: SessionAuth,
                       val access: Access2,
                       cfg: ApiConfig) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "tasks" / id => getTask(req, id)
    case req @ PATCH -> Root / "api" / "tasks" / id => patchTask(req, id)
    case req @ DELETE -> Root / "api" / "tasks" / id => deleteTask(req, id)
    case req @ POST -> Root / "api" / "tasks" / id / "move" => postTaskMove(req, id)
    case req @ POST -> Root / "api" / "tasks" / "bulk" => postTasksBulk(req)
    case req @ GET -> Root / "api" / "tasks" / id / "history" => getTaskHistory(req, id)
    case req @ POST -> Root / "api" / "tasks" / id / "watch" => postTaskWatch(req, id)
    case req @ DELETE -> Root / "api" / "tasks" / id / "watch" => deleteTaskWatch(req, id)
    case req @ POST -> Root / "api" / "tasks" / id / "watchers" => postTaskWatchers(req, id)
    case req @ DELETE -> Root / "api" / "tasks" / id / "watchers" / UuidVar(uid) => deleteTaskWatcher(req, uid, id)
    case req @ GET -> Root / "api" / "tasks" / id / "links" => getTaskLinks(req, id)
    case req @ POST -> Root / "api" / "tasks" / id / "links" => postTaskLinks(req, id)
    case req @ DELETE -> Root / "api" / "tasks" / id / "links" / UuidVar(lid) => deleteTaskLink(req, lid, id)
    case req @ POST -> Root / "api" / "tasks" / id / "dependencies" => postTaskDependencies(req, id)
    case req @ DELETE -> Root / "api" / "tasks" / id / "dependencies" / UuidVar(pred) =>
      deleteTaskDependency(req, pred, id)
    case req @ GET -> Root / "api" / "tasks" / id / "worklogs" => getTaskWorklogs(req, id)
    case req @ POST -> Root / "api" / "tasks" / id / "worklogs" => postTaskWorklogs(req, id)
    case req @ PUT -> Root / "api" / "worklogs" / UuidVar(wid) => putWorklog(req, wid)
    case req @ DELETE -> Root / "api" / "worklogs" / UuidVar(wid) => deleteWorklog(req, wid)
    case req @ GET -> Root / "api" / "tasks" / id / "comments" => getTaskComments(req, id)
    case req @ POST -> Root / "api" / "tasks" / id / "comments" => postTaskComments(req, id)
    case req @ POST -> Root / "api" / "comments" / UuidVar(cid) / "reactions" => postCommentReactions(req, cid)
    case req @ DELETE -> Root / "api" / "comments" / UuidVar(cid) / "reactions" => deleteCommentReactions(req, cid)
    case req @ GET -> Root / "api" / "tasks" / id / "draft" => getTaskDraft(req, id)
    case req @ PUT -> Root / "api" / "tasks" / id / "draft" => putTaskDraft(req, id)
    case req @ DELETE -> Root / "api" / "tasks" / id / "draft" => deleteTaskDraft(req, id)
    case req @ PUT -> Root / "api" / "comments" / UuidVar(cid) => putComment(req, cid)
    case req @ DELETE -> Root / "api" / "comments" / UuidVar(cid) => deleteComment(req, cid)
    case req @ GET -> Root / "api" / "tasks" / id / "attachments" => getTaskAttachments(req, id)
    case req @ POST -> Root / "api" / "tasks" / id / "attachments" => postTaskAttachments(req, id)
    case req @ GET -> Root / "api" / "attachments" / UuidVar(aid) => getAttachment(req, aid)
    case req @ DELETE -> Root / "api" / "attachments" / UuidVar(aid) => deleteAttachment(req, aid)
    case req @ GET -> Root / "api" / "tasks" / id / "automations" => getTaskAutomations(req, id)
    case req @ POST -> Root / "api" / "tasks" / id / "automations" / UuidVar(rid) / "run" =>
      postTaskAutomationRun(req, rid, id)
  }

  /** 見られないプロジェクトのタスクへのリンクは返さない（タイトルなどが漏れないように）。 */
  private def visibleLinks(s: AuthSession, ls: List[TaskLinkDto]): IO[List[TaskLinkDto]] =
    ls.filterA(l => l.target.fold(IO.pure(true))(r => access.task(s, r.id.toString, Role.View).map(_.isRight)))

  private def detail(s: AuthSession, t: TaskCore, role: Role): IO[Response[IO]] =
    tasks.detail(t.id).flatMap {
      case None => err(ErrorCode.TaskNotFound)
      case Some(d) =>
        (activity.markTaskRead(s.user.id, t.id).whenA(s.isSession) *> visibleLinks(s, d.links)).flatMap(links =>
          Ok(TaskDetailDto(
            d.task,
            d.core.description,
            d.core.description.fold("")(RichText.toHtml),
            d.ancestors,
            d.subtasks,
            d.watchers,
            d.watchers.exists(_.id == s.user.id),
            links,
            d.predecessors,
            d.successors,
            d.fields,
            d.commits,
            d.attachments,
            role.asString,
            d.latestEventId
          ))
        )
    }

  /** リアクションを付ける・外す（決まった絵文字だけ。閲覧できる人なら付けられる）。 */
  private def reaction(req: Request[IO], cid: UUID, on: Boolean): IO[Response[IO]] =
    authed(req)(s =>
      tasks.commentAuthor(cid).flatMap {
        case None => err(ErrorCode.CommentNotFound)
        case Some((tid, _)) => respond(access.task(s, tid.toString, Role.View))((t, _) => {
            val emojiName =
              if on then req.as[ReactionReq].attempt.map(_.toOption.map(_.emoji)) else IO.pure(param(req, "emoji"))
            // 決まった絵文字か、登録済みのカスタム絵文字（:名前:）だけ。外すときは何でも外せる
            val usable = emojiName.flatMap {
              case Some(e) if !on || Reactions.all.contains(e) => IO.pure(Some(e))
              case Some(e) =>
                Reactions.customName(e).fold(IO.pure(None: Option[String]))(n => emoji.find(n).map(_.map(_ => e)))
              case None => IO.pure(None)
            }
            usable.flatMap {
              case Some(e) if s.canWrite =>
                tasks.react(cid, s.user.id, e, on) *>
                  tasks.event(
                    t.projectId,
                    Some(t.id),
                    Some(s.user.id),
                    s.viaString,
                    "comment.reacted",
                    Json.obj("commentId" -> cid.asJson)
                  ) *>
                  tasks.comment(cid).flatMap(c => Ok(c.map(_.reactions).getOrElse(Nil)))
              case Some(_) => err(ErrorCode.ApiKeyReadOnly)
              case None => err(ErrorCode.ReactionEmojiInvalid)
            }
          })
      }
    )

  private def getTask(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.View)(detail)

  private def patchTask(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.Edit)((s, t, _) =>
      body[Json](req)(j =>
        svc.parsePatch(j) match {
          case Left(f) => fail(f)
          case Right(ch) => respond(svc.update(Op(s), t, ch))(Ok(_))
        }
      )
    )

  private def deleteTask(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.Edit)((s, t, _) => respond(svc.delete(Op(s), t))(_ => NoContent()))

  private def postTaskMove(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.Edit)((s, t, _) => body[MoveTaskReq](req)(r => respond(svc.move(Op(s), t, r))(Ok(_))))

  private def postTasksBulk(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s =>
      body[BulkUpdateReq](req)(r =>
        if r.taskIds.size > 500 then err(ErrorCode.TaskBulkTooMany)
        else respond(svc.bulk(Op(s), r.taskIds, r.patch))(n => Ok(Json.obj("updated" -> n.asJson)))
      )
    )

  private def getTaskHistory(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.View)((_, t, _) => tasks.history(t.id, intParam(req, "limit", 100, 500)).flatMap(Ok(_)))

  private def postTaskWatch(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.View)((s, t, _) => tasks.watch(t.id, s.user.id) *> NoContent())

  private def deleteTaskWatch(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.View)((s, t, _) => tasks.unwatch(t.id, s.user.id) *> NoContent())

  private def postTaskWatchers(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.Edit)((_, t, _) =>
      body[Json](req)(j =>
        j.hcursor.get[UUID]("userId").toOption match {
          case None => err(ErrorCode.TaskWatcherUserIdRequired)
          case Some(u) => respond(svc.peopleOk(t.projectId, List(u)))(_ => tasks.watch(t.id, u) *> NoContent())
        }
      )
    )

  private def deleteTaskWatcher(req: Request[IO], uid: UUID, id: String): IO[Response[IO]] =
    withTask(req, id, Role.Edit)((_, t, _) => tasks.unwatch(t.id, uid) *> NoContent())

  private def getTaskLinks(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.View)((s, t, _) => tasks.links(t.id).flatMap(visibleLinks(s, _)).flatMap(Ok(_)))

  private def postTaskLinks(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.Edit)((s, t, _) =>
      body[CreateLinkReq](req) { r =>
        (r.targetKey.map(_.trim).filter(_.nonEmpty), r.url.map(_.trim).filter(_.nonEmpty)) match {
          case (Some(k), _) =>
            if !LinkKind.taskKinds.contains(r.kind) then
              err(ErrorCode.TaskLinkKindInvalid, s"リンクの種類は ${LinkKind.taskKinds.mkString(" / ")} です")
            else
              respond(access.task(s, k, Role.View)) {
                (target, _) =>
                  if target.id == t.id then err(ErrorCode.TaskLinkSelf)
                  else
                    tasks.addLink(t.id, r.kind, Some(target.id), None, "", s.user.id).flatMap(lid =>
                      hub.touched(
                        Op(s),
                        t,
                        "links",
                        "link" -> Json.obj("kind" -> r.kind.asJson, "key" -> target.key.asJson)
                      ) *>
                        Created(Json.obj("id" -> lid.asJson))
                    )
              }
          case (None, Some(u)) =>
            RichText.safeUrl(u).filter(x => x.startsWith("http")) match {
              case None => err(ErrorCode.UrlSchemeInvalid)
              case Some(url) =>
                tasks.addLink(
                  t.id,
                  LinkKind.Web,
                  None,
                  Some(url),
                  r.title.map(_.trim).filter(_.nonEmpty).getOrElse(url),
                  s.user.id
                ).flatMap(lid =>
                  hub.touched(Op(s), t, "links") *>
                    Created(Json.obj("id" -> lid.asJson))
                )
            }
          case _ => err(ErrorCode.TaskLinkTargetRequired)
        }
      }
    )

  private def deleteTaskLink(req: Request[IO], lid: UUID, id: String): IO[Response[IO]] =
    withTask(req, id, Role.Edit)((s, t, _) =>
      tasks.deleteLink(t.id, lid).flatMap {
        case true => hub.touched(Op(s), t, "links") *> NoContent()
        case false => err(ErrorCode.TaskLinkNotFound)
      }
    )

  private def postTaskDependencies(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.Edit)((s, t, _) =>
      body[CreateDependencyReq](req)(r =>
        respond(access.task(s, r.predecessorKey, Role.View)) {
          (pred, _) =>
            {
              val kind = r.kind.filter(Set("fs", "ss", "ff", "sf")).getOrElse("fs")
              if pred.projectId != t.projectId then err(ErrorCode.TaskDependencyCrossProject)
              else
                tasks.addDependency(pred.id, t.id, kind).flatMap(_.fold(
                  fail,
                  _ =>
                    hub.touched(Op(s), t, "dependencies", "predecessor" -> pred.key.asJson) *> NoContent()
                ))
            }
        }
      )
    )

  private def deleteTaskDependency(req: Request[IO], pred: UUID, id: String): IO[Response[IO]] =
    withTask(req, id, Role.Edit)((s, t, _) =>
      tasks.deleteDependency(pred, t.id).flatMap {
        case true => hub.touched(Op(s), t, "dependencies") *> NoContent()
        case false => err(ErrorCode.TaskDependencyNotFound)
      }
    )

  private def getTaskWorklogs(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.View)((_, t, _) => tasks.worklogs(t.id).flatMap(Ok(_)))

  private def postTaskWorklogs(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.Edit)((s, t, _) =>
      body[WorklogReq](req)(r => respond(svc.addWorklog(Op(s), t, r))(Created(_)))
    )

  private def putWorklog(req: Request[IO], wid: UUID): IO[Response[IO]] =
    authed(req)(s =>
      tasks.worklog(wid).flatMap {
        case None => err(ErrorCode.WorklogNotFound)
        case Some(w) =>
          respond(access.project(s, w.projectId, Role.Edit)) { role =>
            if w.user.id != s.user.id && role < Role.Admin then err(ErrorCode.WorklogEditNotOwner)
            else
              body[WorklogReq](req)(r =>
                Dates.parse(Some(r.workDate), "作業日") match {
                  case Right(Some(d)) if r.minutes > 0 && r.minutes <= 24 * 60 =>
                    tasks.updateWorklog(wid, d, r.minutes, r.note.getOrElse("").trim) *>
                      hub.emit(
                        Op(s),
                        w.projectId,
                        Some(w.taskId),
                        "worklog.updated",
                        Json.obj("worklogId" -> wid.asJson)
                      ) *> NoContent()
                  case Left(f) => fail(f)
                  case _ => err(ErrorCode.WorklogInputInvalid)
                }
              )
          }
      }
    )

  private def deleteWorklog(req: Request[IO], wid: UUID): IO[Response[IO]] =
    authed(req)(s =>
      tasks.worklog(wid).flatMap {
        case None => err(ErrorCode.WorklogNotFound)
        case Some(w) =>
          respond(access.project(s, w.projectId, Role.Edit)) { role =>
            if w.user.id != s.user.id && role < Role.Admin then err(ErrorCode.WorklogDeleteNotOwner)
            else
              tasks.deleteWorklog(wid) *> hub.emit(
                Op(s),
                w.projectId,
                Some(w.taskId),
                "worklog.deleted",
                Json.obj("worklogId" -> wid.asJson)
              ) *> NoContent()
          }
      }
    )

  private def getTaskComments(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.View)((_, t, _) => tasks.comments(t.id).flatMap(Ok(_)))

  private def postTaskComments(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.Edit)((s, t, _) =>
      body[CommentReq](req)(r =>
        respond(svc.addComment(Op(s), t, r.body, r.internal.getOrElse(false)))(c =>
          tasks.deleteDraft(s.user.id, t.id) *> Created(c)
        )
      )
    )

  private def postCommentReactions(req: Request[IO], cid: UUID): IO[Response[IO]] =
    reaction(req, cid, on = true)

  private def deleteCommentReactions(req: Request[IO], cid: UUID): IO[Response[IO]] =
    reaction(req, cid, on = false)

  private def getTaskDraft(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.View)((s, t, _) => tasks.draft(s.user.id, t.id).flatMap(d => Ok(d.asJson)))

  private def putTaskDraft(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.Edit)((s, t, _) =>
      body[CommentReq](req)(r =>
        if !RichText.valid(r.body) then err(ErrorCode.DraftBodyInvalid)
        else if r.body.noSpaces.length > 1_000_000 then err(ErrorCode.DraftTooLarge)
        else if RichText.isBlank(r.body) then tasks.deleteDraft(s.user.id, t.id) *> NoContent()
        else tasks.saveDraft(s.user.id, t.id, r.body) *> NoContent()
      )
    )

  private def deleteTaskDraft(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.View)((s, t, _) => tasks.deleteDraft(s.user.id, t.id) *> NoContent())

  private def putComment(req: Request[IO], cid: UUID): IO[Response[IO]] =
    authed(req)(s =>
      tasks.commentAuthor(cid).flatMap {
        case None => err(ErrorCode.CommentNotFound)
        case Some((tid, _)) => respond(access.task(s, tid.toString, Role.Edit))((t, _) =>
            body[CommentReq](req)(r => respond(svc.updateComment(Op(s), t, cid, r.body))(Ok(_)))
          )
      }
    )

  private def deleteComment(req: Request[IO], cid: UUID): IO[Response[IO]] =
    authed(req)(s =>
      tasks.commentAuthor(cid).flatMap {
        case None => err(ErrorCode.CommentNotFound)
        case Some((tid, _)) => respond(access.task(s, tid.toString, Role.Edit))((t, role) =>
            respond(svc.deleteComment(Op(s), t, cid, role))(_ => NoContent())
          )
      }
    )

  private def getTaskAttachments(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.View)((_, t, _) => tasks.attachments(t.id).flatMap(Ok(_)))

  private def postTaskAttachments(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.Edit)((s, t, _) =>
      if !files.available then err(ErrorCode.AttachmentStorageNotConfigured)
      else
        withUpload(req, cfg.maxUploadBytes)(up =>
          files.save(Op(s), t, up).flatMap(_.fold(err(ErrorCode.AttachmentStorageNotConfigured))(Created(_)))
        )
    )

  private def getAttachment(req: Request[IO], aid: UUID): IO[Response[IO]] =
    authed(req)(s =>
      tasks.attachment(aid).flatMap {
        case None => err(ErrorCode.AttachmentNotFound)
        case Some((tid, key, name, ct, _)) =>
          respond(access.task(s, tid.toString, Role.View))((_, _) => files.download(key, name, ct))
      }
    )

  private def deleteAttachment(req: Request[IO], aid: UUID): IO[Response[IO]] =
    authed(req)(s =>
      tasks.attachment(aid).flatMap {
        case None => err(ErrorCode.AttachmentNotFound)
        case Some((tid, _, name, _, _)) =>
          respond(access.task(s, tid.toString, Role.Edit))((t, _) =>
            tasks.deleteAttachment(aid).flatMap(k =>
              k.traverse_(key => blobs.traverse_(_.delete(key).handleError(_ => ())))
            ) *>
              hub.touched(Op(s), t, "attachments", "removed" -> name.asJson) *> NoContent()
          )
      }
    )

  private def getTaskAutomations(req: Request[IO], id: String): IO[Response[IO]] =
    withTask(req, id, Role.View)((_, t, _) =>
      integ.rulesFor(t.projectId, "manual").flatMap(rs => Ok(rs.map(r => ManualRuleDto(r.id, r.name))))
    )

  private def postTaskAutomationRun(req: Request[IO], rid: UUID, id: String): IO[Response[IO]] =
    withTask(req, id, Role.Edit)((s, t, _) => respond(automation.manual(Op(s), rid, t))(_ => Accepted(Json.obj())))
}
