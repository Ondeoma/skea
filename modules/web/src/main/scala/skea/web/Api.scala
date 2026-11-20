package skea.web

import io.circe.*
import io.circe.parser.parse
import io.circe.syntax.*
import org.scalajs.dom
import skea.shared.*

import java.util.UUID
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/** バックエンドの REST API（共通部分は sbirds-lib の ApiBase）. */
object Api extends sbirds.web.ApiBase {

  // ---------------------------------------------------------------- 認証・自分

  def clientConfig(): Future[ClientConfigDto] = get[ClientConfigDto]("/api/client-config")
  def me(): Future[MeDto] = get[MeDto]("/api/me")

  /** ログインしていなければ None（ログイン画面へ移らない. ヘルプデスクの窓口の案内はログインせずに見られる）. */
  def meOpt(): Future[Option[MeDto]] =
    dom.fetch("/api/me", init(dom.HttpMethod.GET)).toFuture.flatMap { res =>
      if (!res.ok) Future.successful(None) else res.text().toFuture.map(t => parse(t).flatMap(_.as[MeDto]).toOption)
    }
  def devLogin(name: String, email: String, admin: Boolean, returnTo: String): Future[Json] =
    post[Json](
      "/auth/dev-login",
      Json.obj("name" -> name.asJson, "email" -> email.asJson, "admin" -> admin.asJson, "returnTo" -> returnTo.asJson)
    )

  /** ログアウトして、次に開く URL（IdP のログアウト画面か「/」）を返す. */
  def logout(): Future[String] = post[Json]("/auth/logout").map(_.hcursor.get[String]("redirect").getOrElse("/"))
  def users(q: String): Future[List[UserBrief]] =
    get[List[UserBrief]]("/api/users" + query("q" -> Some(q), "limit" -> Some("20")))

  // ---------------------------------------------------------------- プロジェクト

  def projects(archived: Boolean = false): Future[List[ProjectDto]] =
    get[List[ProjectDto]]("/api/projects" + (if (archived) "?archived=true" else ""))
  def projectByKey(key: String): Future[ProjectDto] = get[ProjectDto](s"/api/projects/by-key/${enc(key)}")
  def meta(pid: UUID): Future[ProjectMetaDto] = get[ProjectMetaDto](s"/api/projects/$pid/meta")
  def audience(pid: UUID): Future[List[UserBrief]] = get[List[UserBrief]](s"/api/projects/$pid/audience")
  def board(pid: UUID): Future[BoardDto] = get[BoardDto](s"/api/projects/$pid/board")
  def backlog(pid: UUID): Future[BacklogDto] = get[BacklogDto](s"/api/projects/$pid/backlog")
  def gantt(pid: UUID, done: Boolean): Future[GanttDto] = get[GanttDto](s"/api/projects/$pid/gantt?done=$done")
  def changes(pid: UUID, since: Long): Future[ChangesDto] = get[ChangesDto](s"/api/projects/$pid/changes?since=$since")
  def activity(pid: UUID): Future[List[EventDto]] = get[List[EventDto]](s"/api/projects/$pid/activity?limit=50")
  def sprints(pid: UUID, closed: Boolean): Future[List[SprintDto]] =
    get[List[SprintDto]](s"/api/projects/$pid/sprints?closed=$closed")

  // ---------------------------------------------------------------- タスク

  def task(key: String): Future[TaskDetailDto] = get[TaskDetailDto](s"/api/tasks/${enc(key)}")
  def createTask(pid: UUID, r: CreateTaskReq): Future[TaskDto] = post[TaskDto](s"/api/projects/$pid/tasks", r.asJson)
  def updateTask(id: UUID, patch: Json): Future[TaskDto] = this.patch[TaskDto](s"/api/tasks/$id", patch)
  def moveTask(id: UUID, r: MoveTaskReq): Future[TaskDto] =
    post[TaskDto](s"/api/tasks/$id/move", r.asJson.dropNullValues)
  def deleteTask(id: UUID): Future[Unit] = delete(s"/api/tasks/$id")
  def comments(id: UUID): Future[List[CommentDto]] = get[List[CommentDto]](s"/api/tasks/$id/comments")
  def addComment(id: UUID, body: Json, internal: Boolean = false): Future[CommentDto] =
    post[CommentDto](s"/api/tasks/$id/comments", CommentReq(body, Some(internal)).asJson)
  def updateComment(cid: UUID, body: Json): Future[CommentDto] =
    put[CommentDto](s"/api/comments/$cid", Json.obj("body" -> body))
  def worklogs(id: UUID): Future[List[WorklogDto]] = get[List[WorklogDto]](s"/api/tasks/$id/worklogs")
  def addWorklog(id: UUID, r: WorklogReq): Future[WorklogDto] = post[WorklogDto](s"/api/tasks/$id/worklogs", r.asJson)
  def history(id: UUID): Future[List[EventDto]] = get[List[EventDto]](s"/api/tasks/$id/history?limit=100")
  def draft(id: UUID): Future[Option[DraftDto]] = get[Option[DraftDto]](s"/api/tasks/$id/draft")
  def saveDraft(id: UUID, body: Json): Future[Unit] = putUnit(s"/api/tasks/$id/draft", Json.obj("body" -> body))
  def addWatcher(id: UUID, userId: UUID): Future[Unit] =
    postUnit(s"/api/tasks/$id/watchers", Json.obj("userId" -> userId.asJson))
  def removeWatcher(id: UUID, userId: UUID): Future[Unit] = delete(s"/api/tasks/$id/watchers/$userId")
  def manualRules(id: UUID): Future[List[ManualRuleDto]] = get[List[ManualRuleDto]](s"/api/tasks/$id/automations")

  /** 添付ファイルのアップロード. */
  def upload(taskId: UUID, file: dom.File): Future[AttachmentDto] =
    postFile[AttachmentDto](s"/api/tasks/$taskId/attachments", file, file.name)

  // ---------------------------------------------------------------- ヘルプデスク（窓口）

  def helpProjects(): Future[List[HelpPortalDto]] = get[List[HelpPortalDto]]("/api/help/projects")
  def createRequest(pid: UUID, r: HelpRequestReq): Future[HelpTaskDto] =
    post[HelpTaskDto](s"/api/help/projects/$pid/tasks", r.asJson)
  def helpTask(key: String): Future[HelpTaskDetailDto] = get[HelpTaskDetailDto](s"/api/help/tasks/${enc(key)}")
  def helpReply(key: String, body: Json): Future[CommentDto] =
    post[CommentDto](s"/api/help/tasks/${enc(key)}/comments", CommentReq(body).asJson)
  def helpUpload(taskId: UUID, file: dom.File): Future[AttachmentDto] =
    postFile[AttachmentDto](s"/api/help/tasks/$taskId/attachments", file, file.name)

  /** 窓口の案内（ログインしていなくても見られる. 見られなければ None）. */
  def portal(key: String): Future[Option[HelpPortalDto]] =
    dom.fetch(s"/api/help/portal/${enc(key)}", init(dom.HttpMethod.GET)).toFuture.flatMap { res =>
      if (!res.ok) Future.successful(None)
      else res.text().toFuture.map(t => parse(t).flatMap(_.as[HelpPortalDto]).toOption)
    }
  def joinPortal(key: String): Future[HelpPortalDto] = post[HelpPortalDto](s"/api/help/portal/${enc(key)}/join")

  // ---------------------------------------------------------------- 検索・通知・作業時間

  def search(q: String, projectId: Option[UUID], limit: Int = 100, offset: Int = 0): Future[SearchResultDto] =
    get[SearchResultDto]("/api/search" + query(
      "q" -> Some(q),
      "projectId" -> projectId.map(_.toString),
      "limit" -> Some(limit.toString),
      "offset" -> Some(offset.toString)
    ))
  def filters(): Future[List[SavedFilterDto]] = get[List[SavedFilterDto]]("/api/filters")
  def notifications(kinds: List[String], unread: Boolean): Future[NotificationListDto] =
    get[NotificationListDto]("/api/notifications" +
      query("kinds" -> Some(kinds.mkString(",")), "unread" -> Option.when(unread)("true"), "limit" -> Some("100")))
  def unread(): Future[UnreadDto] = get[UnreadDto]("/api/notifications/unread")
  def worklogReport(from: String, to: String, projectId: Option[UUID], userId: Option[UUID]): Future[List[WorklogDto]] =
    get[List[WorklogDto]]("/api/worklogs" + query(
      "from" -> Some(from),
      "to" -> Some(to),
      "projectId" -> projectId.map(_.toString),
      "userId" -> userId.map(_.toString)
    ))

}
