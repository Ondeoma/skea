package skea.api

import sbirds.observe.Log

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import skea.core.*
import skea.db.*
import skea.shared.*

import java.util.UUID

/** 変更が起きたときの共通処理: 変更履歴（画面の同期）・Webhook・自動化の起動。 */
final class EventHub(tasks: TaskStore, projects: ProjectStore, integ: IntegrationStore) {

  /** 自動化の連鎖の上限。 */
  val MaxDepth = 5

  def emit(op: Op, projectId: UUID, taskId: Option[UUID], kind: String, detail: Json): IO[Long] =
    for {
      id <- tasks.event(projectId, taskId, Some(op.userId), op.via, kind, detail)
      _ <- fanOut(op, projectId, taskId, kind, detail).handleErrorWith(e =>
        Log.report(ErrorCode.EventFanoutFailed, kind, e)
      )
    } yield id

  /** タスクに付くもの（リンク・依存関係・添付）が変わった（task.updated。changes に field を入れる）。 */
  def touched(op: Op, t: TaskCore, field: String, extra: (String, Json)*): IO[Unit] =
    emit(
      op,
      t.projectId,
      Some(t.id),
      EventKind.TaskUpdated,
      Json.obj((("changes" -> Json.arr(Json.obj("field" -> field.asJson))) +: extra)*)
    ).void

  /** システムの操作（SCM など、操作者のいないもの）。 */
  def emitSystem(projectId: UUID, taskId: Option[UUID], via: String, kind: String, detail: Json): IO[Long] =
    tasks.event(projectId, taskId, None, via, kind, detail).flatTap(_ =>
      fanOut(None, projectId, taskId, kind, detail, 0).handleErrorWith(e =>
        Log.report(ErrorCode.EventFanoutFailed, kind, e)
      )
    )

  private def fanOut(op: Op, projectId: UUID, taskId: Option[UUID], kind: String, detail: Json): IO[Unit] =
    fanOut(Some(op.s.user), projectId, taskId, kind, detail, op.depth)

  private def fanOut(actor: Option[User],
                     projectId: UUID,
                     taskId: Option[UUID],
                     kind: String,
                     detail: Json,
                     depth: Int): IO[Unit] =
    for {
      p <- projects.find(projectId)
      task <- taskId.flatTraverse(tasks.dto)
      payload = EventHub.payload(kind, p, task, actor.map(u => (u.id, u.name)), detail)
      _ <- integ.enqueueDeliveries(projectId, kind, payload)
      _ <- Automation.Trigger.eventType(kind) match {
        case Some(tpe) if depth < MaxDepth =>
          integ.rulesFor(projectId, tpe).flatMap(_.traverse_(r =>
            integ.enqueueRun(r.id, taskId, actor.map(_.id), payload, depth)
          ))
        case _ => IO.unit
      }
    } yield ()
}

object EventHub {

  /** Webhook で送る本文（自動化にも渡す）。 */
  def payload(kind: String,
              p: Option[Project],
              task: Option[TaskDto],
              actor: Option[(UUID, String)],
              detail: Json,
              at: java.time.Instant = java.time.Instant.now()): Json =
    Json.obj(
      "event" -> kind.asJson,
      "project" ->
        p.fold(Json.Null)(x => Json.obj("id" -> x.id.asJson, "key" -> x.key.asJson, "name" -> x.name.asJson)),
      "task" -> task.asJson,
      "actor" -> actor.fold(Json.Null)((id, name) => Json.obj("id" -> id.asJson, "name" -> name.asJson)),
      "detail" -> detail,
      "at" -> at.asJson
    )

  /** Webhook の本文の見本（設定画面で表示する）。値は架空のもの。 */
  def sample(kind: String, p: Project, actor: (UUID, String)): Json = {
    val now = java.time.Instant.now()
    val tid = UUID.fromString("00000000-0000-4000-8000-000000000001")
    val st = UUID.fromString("00000000-0000-4000-8000-000000000002")
    val key = s"${p.key}-123"
    val user = UserBrief(actor._1, actor._2, None)
    val task = TaskDto(
      tid,
      key,
      p.id,
      None,
      "ログイン画面を作る",
      st,
      None,
      Some(user),
      Some(user),
      Some("2026-11-10"),
      Some("2026-11-20"),
      90,
      "a0",
      Nil,
      2,
      1,
      3,
      4,
      now,
      now,
      None,
      Priority.High,
      TaskType.Story
    )
    val detail = kind match {
      case EventKind.TaskCreated => Json.obj("title" -> task.title.asJson, "key" -> key.asJson)
      case EventKind.TaskUpdated => Json.obj("changes" -> Json.arr(
          Json.obj("field" -> "dueDate".asJson, "from" -> "2026-11-20".asJson, "to" -> "2026-11-25".asJson),
          Json.obj("field" -> "assigneeId".asJson, "from" -> Json.Null, "to" -> actor._1.asJson)
        ))
      case EventKind.TaskDeleted => Json.obj(
          "taskId" -> tid.asJson,
          "key" -> key.asJson,
          "title" -> task.title.asJson,
          "deleted" -> List(tid).asJson
        )
      case EventKind.StatusChanged => Json.obj(
          "from" -> "作業前".asJson,
          "to" -> "進行中".asJson,
          "fromCategory" -> "todo".asJson,
          "toCategory" -> "in_progress".asJson
        )
      case EventKind.CommentAdded => Json.obj(
          "commentId" -> UUID.fromString("00000000-0000-4000-8000-000000000003").asJson,
          "text" -> "確認しました。明日対応します。".asJson
        )
      case EventKind.CommentUpdated | EventKind.CommentDeleted =>
        Json.obj("commentId" -> UUID.fromString("00000000-0000-4000-8000-000000000003").asJson)
      case EventKind.WorklogAdded => Json.obj(
          "worklogId" -> UUID.fromString("00000000-0000-4000-8000-000000000004").asJson,
          "minutes" -> 90.asJson,
          "workDate" -> "2026-11-15".asJson
        )
      case EventKind.SprintCreated | EventKind.SprintStarted => Json.obj(
          "sprintId" -> UUID.fromString("00000000-0000-4000-8000-000000000005").asJson,
          "name" -> "スプリント 3".asJson
        )
      case EventKind.SprintCompleted => Json.obj(
          "sprintId" -> UUID.fromString("00000000-0000-4000-8000-000000000005").asJson,
          "moved" -> 2.asJson,
          "moveTo" -> Json.Null
        )
      case EventKind.CommitLinked => Json.obj(
          "sha" -> "3f9c2d1e8a7b6c5d4e3f2a1b0c9d8e7f6a5b4c3d".asJson,
          "message" -> s"$key ログイン画面を追加".asJson,
          "url" -> "https://github.com/example/app/commit/3f9c2d1e".asJson,
          "author" -> actor._2.asJson,
          "repo" -> "example/app".asJson,
          "branch" -> "main".asJson
        )
      case EventKind.AttachmentAdded => Json.obj(
          "attachmentId" -> UUID.fromString("00000000-0000-4000-8000-000000000006").asJson,
          "fileName" -> "画面設計.png".asJson
        )
      case _ => Json.obj()
    }
    val withTask =
      !Set(EventKind.SprintCreated, EventKind.SprintStarted, EventKind.SprintCompleted, EventKind.TaskDeleted)(kind)
    payload(kind, Some(p), Option.when(withTask)(task), Some(actor), detail, now)
  }
}
