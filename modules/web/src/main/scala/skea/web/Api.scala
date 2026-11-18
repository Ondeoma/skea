package skea.web

import io.circe.*
import io.circe.parser.parse
import io.circe.syntax.*
import org.scalajs.dom
import skea.shared.*

import java.util.UUID
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

/** API の呼び出しのエラー. */
final case class ApiError(status: Int, message: String) extends Exception(message)

/** バックエンドの REST API. 401 はログイン画面へ、エラーは {"error": ...} の文言にする. */
object Api {

  /** fetch の設定（同じオリジンの Cookie を付ける. body は JSON の文字列）. */
  def init(method: dom.HttpMethod, body: Option[String] = None): dom.RequestInit = {
    val i = new dom.RequestInit {}
    i.method = method
    i.credentials = dom.RequestCredentials.`same-origin`
    body.foreach { b =>
      i.headers = js.Dictionary("Content-Type" -> "application/json")
      i.body = b
    }
    i
  }

  private def request(method: dom.HttpMethod, url: String, body: Option[Json] = None): Future[Option[Json]] =
    dom.fetch(url, init(method, body.map(_.noSpaces))).toFuture.flatMap { res =>
      if (res.status == 401) {
        val back = dom.window.location.pathname + dom.window.location.search
        if (!dom.window.location.pathname.startsWith("/login"))
          dom.window.location.href = "/auth/login?returnTo=" + js.URIUtils.encodeURIComponent(back)
        Future.failed(ApiError(401, "ログインしてください"))
      } else if (!res.ok) {
        res.text().toFuture.recover(_ => "").flatMap { t =>
          val msg = parse(t).toOption.flatMap(_.hcursor.get[String]("error").toOption).getOrElse(s"${res.status} ${res.statusText}")
          Future.failed(ApiError(res.status, msg))
        }
      } else if (res.status == 204) Future.successful(None)
      else res.text().toFuture.map(t => if (t.isEmpty) None else Some(parse(t).fold(throw _, identity)))
    }

  private def as[A: Decoder](j: Option[Json]): A = j.getOrElse(Json.Null).as[A].fold(e => throw ApiError(0, e.getMessage), identity)

  def get[A: Decoder](url: String): Future[A] = request(dom.HttpMethod.GET, url).map(as[A])
  def post[A: Decoder](url: String, body: Json = Json.obj()): Future[A] = request(dom.HttpMethod.POST, url, Some(body)).map(as[A])
  def put[A: Decoder](url: String, body: Json): Future[A] = request(dom.HttpMethod.PUT, url, Some(body)).map(as[A])
  def patch[A: Decoder](url: String, body: Json): Future[A] = request(dom.HttpMethod.PATCH, url, Some(body)).map(as[A])
  def send(method: dom.HttpMethod, url: String, body: Option[Json] = None): Future[Unit] = request(method, url, body).map(_ => ())
  def delete(url: String): Future[Unit] = send(dom.HttpMethod.DELETE, url)
  def postUnit(url: String, body: Json = Json.obj()): Future[Unit] = send(dom.HttpMethod.POST, url, Some(body))
  def putUnit(url: String, body: Json): Future[Unit] = send(dom.HttpMethod.PUT, url, Some(body))
  def patchUnit(url: String, body: Json): Future[Unit] = send(dom.HttpMethod.PATCH, url, Some(body))

  def enc(s: String): String = js.URIUtils.encodeURIComponent(s)

  def query(params: (String, Option[String])*): String = {
    val q = params.collect { case (k, Some(v)) if v.nonEmpty => s"$k=${enc(v)}" }
    if (q.isEmpty) "" else q.mkString("?", "&", "")
  }

  // ---------------------------------------------------------------- 認証・自分

  def clientConfig(): Future[ClientConfigDto] = get[ClientConfigDto]("/api/client-config")
  def me(): Future[MeDto] = get[MeDto]("/api/me")

  /** ログインしていなければ None（ログイン画面へ移らない. ヘルプデスクの窓口の案内はログインせずに見られる）. */
  def meOpt(): Future[Option[MeDto]] =
    dom.fetch("/api/me", init(dom.HttpMethod.GET)).toFuture.flatMap { res =>
      if (!res.ok) Future.successful(None) else res.text().toFuture.map(t => parse(t).flatMap(_.as[MeDto]).toOption)
    }
  def devLogin(name: String, email: String, admin: Boolean, returnTo: String): Future[Json] =
    post[Json]("/auth/dev-login", Json.obj("name" -> name.asJson, "email" -> email.asJson, "admin" -> admin.asJson, "returnTo" -> returnTo.asJson))
  /** ログアウトして、次に開く URL（IdP のログアウト画面か「/」）を返す. */
  def logout(): Future[String] = post[Json]("/auth/logout").map(_.hcursor.get[String]("redirect").getOrElse("/"))
  def users(q: String): Future[List[UserBrief]] = get[List[UserBrief]]("/api/users" + query("q" -> Some(q), "limit" -> Some("20")))

  // ---------------------------------------------------------------- プロジェクト

  def projects(archived: Boolean = false): Future[List[ProjectDto]] = get[List[ProjectDto]]("/api/projects" + (if (archived) "?archived=true" else ""))
  def projectByKey(key: String): Future[ProjectDto] = get[ProjectDto](s"/api/projects/by-key/${enc(key)}")
  def meta(pid: UUID): Future[ProjectMetaDto] = get[ProjectMetaDto](s"/api/projects/$pid/meta")
  def audience(pid: UUID): Future[List[UserBrief]] = get[List[UserBrief]](s"/api/projects/$pid/audience")
  def board(pid: UUID): Future[BoardDto] = get[BoardDto](s"/api/projects/$pid/board")
  def backlog(pid: UUID): Future[BacklogDto] = get[BacklogDto](s"/api/projects/$pid/backlog")
  def gantt(pid: UUID, done: Boolean): Future[GanttDto] = get[GanttDto](s"/api/projects/$pid/gantt?done=$done")
  def changes(pid: UUID, since: Long): Future[ChangesDto] = get[ChangesDto](s"/api/projects/$pid/changes?since=$since")
  def activity(pid: UUID): Future[List[EventDto]] = get[List[EventDto]](s"/api/projects/$pid/activity?limit=50")
  def sprints(pid: UUID, closed: Boolean): Future[List[SprintDto]] = get[List[SprintDto]](s"/api/projects/$pid/sprints?closed=$closed")

  // ---------------------------------------------------------------- タスク

  def task(key: String): Future[TaskDetailDto] = get[TaskDetailDto](s"/api/tasks/${enc(key)}")
  def createTask(pid: UUID, r: CreateTaskReq): Future[TaskDto] = post[TaskDto](s"/api/projects/$pid/tasks", r.asJson)
  def updateTask(id: UUID, patch: Json): Future[TaskDto] = this.patch[TaskDto](s"/api/tasks/$id", patch)
  def moveTask(id: UUID, r: MoveTaskReq): Future[TaskDto] = post[TaskDto](s"/api/tasks/$id/move", r.asJson.dropNullValues)
  def deleteTask(id: UUID): Future[Unit] = delete(s"/api/tasks/$id")
  def comments(id: UUID): Future[List[CommentDto]] = get[List[CommentDto]](s"/api/tasks/$id/comments")
  def addComment(id: UUID, body: Json, internal: Boolean = false): Future[CommentDto] =
    post[CommentDto](s"/api/tasks/$id/comments", CommentReq(body, Some(internal)).asJson)
  def updateComment(cid: UUID, body: Json): Future[CommentDto] = put[CommentDto](s"/api/comments/$cid", Json.obj("body" -> body))
  def worklogs(id: UUID): Future[List[WorklogDto]] = get[List[WorklogDto]](s"/api/tasks/$id/worklogs")
  def addWorklog(id: UUID, r: WorklogReq): Future[WorklogDto] = post[WorklogDto](s"/api/tasks/$id/worklogs", r.asJson)
  def history(id: UUID): Future[List[EventDto]] = get[List[EventDto]](s"/api/tasks/$id/history?limit=100")
  def draft(id: UUID): Future[Option[DraftDto]] = get[Option[DraftDto]](s"/api/tasks/$id/draft")
  def saveDraft(id: UUID, body: Json): Future[Unit] = putUnit(s"/api/tasks/$id/draft", Json.obj("body" -> body))
  def addWatcher(id: UUID, userId: UUID): Future[Unit] = postUnit(s"/api/tasks/$id/watchers", Json.obj("userId" -> userId.asJson))
  def removeWatcher(id: UUID, userId: UUID): Future[Unit] = delete(s"/api/tasks/$id/watchers/$userId")
  def manualRules(id: UUID): Future[List[ManualRuleDto]] = get[List[ManualRuleDto]](s"/api/tasks/$id/automations")

  /** ファイルを multipart/form-data（file）で送る. */
  def sendFile[A: Decoder](url: String, file: dom.File): Future[A] = {
    val fd = new dom.FormData()
    fd.append("file", file, file.name)
    val i = init(dom.HttpMethod.POST)
    i.body = fd
    dom.fetch(url, i).toFuture.flatMap { res =>
      res.text().toFuture.map { t =>
        val j = parse(t).getOrElse(Json.Null)
        if (res.ok) j.as[A].fold(e => throw ApiError(0, e.getMessage), identity)
        else throw ApiError(res.status, j.hcursor.get[String]("error").getOrElse(res.statusText))
      }
    }
  }

  /**
   * 外部サービスのリンクの名前（LinkTitles が裏で問い合わせる）. 失敗してもログイン画面には移らない（次に描き直されたときにもう一度試す）.
   */
  def linkTitles(provider: String, ids: List[String]): Future[LinkTitlesDto] =
    dom.fetch(s"/api/integrations/$provider/titles", init(dom.HttpMethod.POST, Some(Json.obj("ids" -> ids.asJson).noSpaces))).toFuture.flatMap { res =>
      if (!res.ok) Future.failed(ApiError(res.status, res.statusText))
      else res.text().toFuture.map(t => parse(t).flatMap(_.as[LinkTitlesDto]).fold(throw _, identity))
    }

  /** 添付ファイルのアップロード. */
  def upload(taskId: UUID, file: dom.File): Future[AttachmentDto] = sendFile[AttachmentDto](s"/api/tasks/$taskId/attachments", file)

  // ---------------------------------------------------------------- ヘルプデスク（窓口）

  def helpProjects(): Future[List[HelpPortalDto]] = get[List[HelpPortalDto]]("/api/help/projects")
  def createRequest(pid: UUID, r: HelpRequestReq): Future[HelpTaskDto] = post[HelpTaskDto](s"/api/help/projects/$pid/tasks", r.asJson)
  def helpTask(key: String): Future[HelpTaskDetailDto] = get[HelpTaskDetailDto](s"/api/help/tasks/${enc(key)}")
  def helpReply(key: String, body: Json): Future[CommentDto] = post[CommentDto](s"/api/help/tasks/${enc(key)}/comments", CommentReq(body).asJson)
  def helpUpload(taskId: UUID, file: dom.File): Future[AttachmentDto] = sendFile[AttachmentDto](s"/api/help/tasks/$taskId/attachments", file)

  /** 窓口の案内（ログインしていなくても見られる. 見られなければ None）. */
  def portal(key: String): Future[Option[HelpPortalDto]] =
    dom.fetch(s"/api/help/portal/${enc(key)}", init(dom.HttpMethod.GET)).toFuture.flatMap { res =>
      if (!res.ok) Future.successful(None) else res.text().toFuture.map(t => parse(t).flatMap(_.as[HelpPortalDto]).toOption)
    }
  def joinPortal(key: String): Future[HelpPortalDto] = post[HelpPortalDto](s"/api/help/portal/${enc(key)}/join")

  // ---------------------------------------------------------------- 検索・通知・作業時間

  def search(q: String, projectId: Option[UUID], limit: Int = 100, offset: Int = 0): Future[SearchResultDto] =
    get[SearchResultDto]("/api/search" + query("q" -> Some(q), "projectId" -> projectId.map(_.toString), "limit" -> Some(limit.toString),
      "offset" -> Some(offset.toString)))
  def filters(): Future[List[SavedFilterDto]] = get[List[SavedFilterDto]]("/api/filters")
  def notifications(kinds: List[String], unread: Boolean): Future[NotificationListDto] =
    get[NotificationListDto]("/api/notifications" + query("kinds" -> Some(kinds.mkString(",")), "unread" -> Option.when(unread)("true"), "limit" -> Some("100")))
  def unread(): Future[UnreadDto] = get[UnreadDto]("/api/notifications/unread")
  def worklogReport(from: String, to: String, projectId: Option[UUID], userId: Option[UUID]): Future[List[WorklogDto]] =
    get[List[WorklogDto]]("/api/worklogs" + query("from" -> Some(from), "to" -> Some(to), "projectId" -> projectId.map(_.toString),
      "userId" -> userId.map(_.toString)))

}
