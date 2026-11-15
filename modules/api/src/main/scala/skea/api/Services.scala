package skea.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import skea.core.*
import skea.db.*
import skea.notify.{Mail, Mailer}
import skea.shared.*

import java.time.LocalDate
import java.util.UUID
import scala.concurrent.duration.*

/** 変更が起きたときの共通処理: 変更履歴（画面の同期）・Webhook・自動化の起動。 */
final class EventHub(tasks: TaskStore, projects: ProjectStore, integ: IntegrationStore):

  /** 自動化の連鎖の上限。 */
  val MaxDepth = 5

  def emit(op: Op, projectId: UUID, taskId: Option[UUID], kind: String, detail: Json): IO[Long] =
    for
      id <- tasks.event(projectId, taskId, Some(op.userId), op.via, kind, detail)
      _  <- fanOut(op, projectId, taskId, kind, detail).handleErrorWith(e => IO.println(s"[skea] event fan-out failed ($kind): $e"))
    yield id

  /** タスクに付くもの（リンク・依存関係・添付）が変わった（task.updated。changes に field を入れる）。 */
  def touched(op: Op, t: TaskCore, field: String, extra: (String, Json)*): IO[Unit] =
    emit(op, t.projectId, Some(t.id), EventKind.TaskUpdated, Json.obj((("changes" -> Json.arr(Json.obj("field" -> field.asJson))) +: extra)*)).void

  /** システムの操作（SCM など、操作者のいないもの）。 */
  def emitSystem(projectId: UUID, taskId: Option[UUID], via: String, kind: String, detail: Json): IO[Long] =
    tasks.event(projectId, taskId, None, via, kind, detail).flatTap(_ => fanOut(None, projectId, taskId, kind, detail, 0).handleErrorWith(e =>
      IO.println(s"[skea] event fan-out failed ($kind): $e")))

  private def fanOut(op: Op, projectId: UUID, taskId: Option[UUID], kind: String, detail: Json): IO[Unit] =
    fanOut(Some(op.s.user), projectId, taskId, kind, detail, op.depth)

  private def fanOut(actor: Option[User], projectId: UUID, taskId: Option[UUID], kind: String, detail: Json, depth: Int): IO[Unit] =
    for
      p    <- projects.find(projectId)
      task <- taskId.flatTraverse(tasks.dto)
      payload = EventHub.payload(kind, p, task, actor.map(u => (u.id, u.name)), detail)
      _ <- integ.enqueueDeliveries(projectId, kind, payload)
      _ <- Automation.Trigger.eventType(kind) match
             case Some(tpe) if depth < MaxDepth =>
               integ.rulesFor(projectId, tpe).flatMap(_.traverse_(r => integ.enqueueRun(r.id, taskId, actor.map(_.id), payload, depth)))
             case _ => IO.unit
    yield ()

object EventHub:

  /** Webhook で送る本文（自動化にも渡す）。 */
  def payload(kind: String, p: Option[Project], task: Option[TaskDto], actor: Option[(UUID, String)], detail: Json,
      at: java.time.Instant = java.time.Instant.now()): Json =
    Json.obj(
      "event"   -> kind.asJson,
      "project" -> p.fold(Json.Null)(x => Json.obj("id" -> x.id.asJson, "key" -> x.key.asJson, "name" -> x.name.asJson)),
      "task"    -> task.asJson,
      "actor"   -> actor.fold(Json.Null)((id, name) => Json.obj("id" -> id.asJson, "name" -> name.asJson)),
      "detail"  -> detail,
      "at"      -> at.asJson
    )

  /** Webhook の本文の見本（設定画面で表示する）。値は架空のもの。 */
  def sample(kind: String, p: Project, actor: (UUID, String)): Json =
    val now  = java.time.Instant.now()
    val tid  = UUID.fromString("00000000-0000-4000-8000-000000000001")
    val st   = UUID.fromString("00000000-0000-4000-8000-000000000002")
    val key  = s"${p.key}-123"
    val user = UserBrief(actor._1, actor._2, None)
    val task = TaskDto(tid, key, p.id, None, "ログイン画面を作る", st, None, Some(user), Some(user), Some("2026-11-10"), Some("2026-11-20"),
      90, "a0", Nil, 2, 1, 3, 4, now, now, None, Priority.High, TaskType.Story)
    val detail = kind match
      case EventKind.TaskCreated => Json.obj("title" -> task.title.asJson, "key" -> key.asJson)
      case EventKind.TaskUpdated => Json.obj("changes" -> Json.arr(
          Json.obj("field" -> "dueDate".asJson, "from" -> "2026-11-20".asJson, "to" -> "2026-11-25".asJson),
          Json.obj("field" -> "assigneeId".asJson, "from" -> Json.Null, "to" -> actor._1.asJson)))
      case EventKind.TaskDeleted => Json.obj("taskId" -> tid.asJson, "key" -> key.asJson, "title" -> task.title.asJson, "deleted" -> List(tid).asJson)
      case EventKind.StatusChanged => Json.obj("from" -> "作業前".asJson, "to" -> "進行中".asJson, "fromCategory" -> "todo".asJson,
          "toCategory" -> "in_progress".asJson)
      case EventKind.CommentAdded => Json.obj("commentId" -> UUID.fromString("00000000-0000-4000-8000-000000000003").asJson,
          "text" -> "確認しました。明日対応します。".asJson)
      case EventKind.CommentUpdated | EventKind.CommentDeleted => Json.obj("commentId" -> UUID.fromString("00000000-0000-4000-8000-000000000003").asJson)
      case EventKind.WorklogAdded => Json.obj("worklogId" -> UUID.fromString("00000000-0000-4000-8000-000000000004").asJson, "minutes" -> 90.asJson,
          "workDate" -> "2026-11-15".asJson)
      case EventKind.SprintCreated | EventKind.SprintStarted => Json.obj("sprintId" -> UUID.fromString("00000000-0000-4000-8000-000000000005").asJson,
          "name" -> "スプリント 3".asJson)
      case EventKind.SprintCompleted => Json.obj("sprintId" -> UUID.fromString("00000000-0000-4000-8000-000000000005").asJson, "moved" -> 2.asJson,
          "moveTo" -> Json.Null)
      case EventKind.CommitLinked => Json.obj("sha" -> "3f9c2d1e8a7b6c5d4e3f2a1b0c9d8e7f6a5b4c3d".asJson, "message" -> s"$key ログイン画面を追加".asJson,
          "url" -> "https://github.com/example/app/commit/3f9c2d1e".asJson, "author" -> actor._2.asJson, "repo" -> "example/app".asJson,
          "branch" -> "main".asJson)
      case EventKind.AttachmentAdded => Json.obj("attachmentId" -> UUID.fromString("00000000-0000-4000-8000-000000000006").asJson,
          "fileName" -> "画面設計.png".asJson)
      case _ => Json.obj()
    val withTask = !Set(EventKind.SprintCreated, EventKind.SprintStarted, EventKind.SprintCompleted, EventKind.TaskDeleted)(kind)
    payload(kind, Some(p), Option.when(withTask)(task), Some(actor), detail, now)

/** 通知（画面内・メール）。 */
final class NotifyService(activity: ActivityStore, projects: ProjectStore, users: UserStore, sessions: SessionAuth, cfg: ApiConfig):

  /** users（操作した本人を除く）に通知する。そのプロジェクトを閲覧できない人には送らない。 */
  def send(kind: String, to: Set[UUID], actor: Option[UUID], projectId: UUID, taskId: Option[UUID], commentId: Option[UUID],
      title: String, body: String, link: String, mailDelay: Int = 0, dedupe: Option[String] = None): IO[Unit] =
    val targets = (to -- actor.toSet).toList
    IO.whenA(targets.nonEmpty) {
      actor.flatTraverse(a => users.find(a).map(_.map(_.name))).flatMap { actorName =>
        val mailBody = List(
          actorName.fold("")(n => s"$n さんの操作です。\n\n"),
          title, "", body.take(2000), "", s"${cfg.publicUrl}$link", "",
          "-- ", "skea（通知の設定は画面右上のメニュー →「個人設定」で変えられます）"
        ).mkString("\n")
        targets.traverse_ { uid =>
          // そのプロジェクトを閲覧できる有効なユーザーだけに送る
          sessions.sessionFor(uid).flatMap(_.flatTraverse(s => projects.role(projectId, s))).flatMap {
            case None => IO.unit
            case Some(_) =>
              activity.notify(uid, kind, Some(projectId), taskId, commentId, actor, title, body) *>
                activity.enqueueMail(Some(uid), None, Some(projectId), s"[skea] $title", mailBody, dedupe.map(d => s"$d:$uid"), mailDelay)
          }
        }
      }
    }.handleErrorWith(e => IO.println(s"[skea] notify failed: $e"))

  /** メールの送信ワーカー（30秒ごと）。送信時点でまだ閲覧できるかを確かめる。 */
  def mailLoop(mailer: Option[Mailer]): IO[Nothing] =
    Periodic.run(20.seconds, 30.seconds)(e => s"[skea] mail worker failed: $e") {
      mailer match
        case None => activity.processMail(50)(_ => IO.pure(OutboxResult.Skip("SMTP が設定されていません")))
        case Some(m) =>
          activity.processMail(50) { row =>
            (row.userId, row.toAddress) match
              case (Some(uid), _) =>
                activity.mailTarget(uid, None).flatMap {
                  case None => IO.pure(OutboxResult.Skip("宛先のユーザーが無効か、メールアドレスがありません"))
                  case Some((addr, name)) =>
                    val stillVisible = row.projectId.fold(IO.pure(true))(pid =>
                      sessions.sessionFor(uid).flatMap(_.fold(IO.pure(false))(s => projects.role(pid, s).map(_.isDefined))))
                    stillVisible.ifM(
                      m.send(Mail(addr, name, row.subject, row.body)).as(OutboxResult.Sent),
                      IO.pure(OutboxResult.Skip("宛先がプロジェクトを閲覧できなくなりました"))
                    )
                }
              case (None, Some(addr)) => m.send(Mail(addr, addr, row.subject, row.body)).as(OutboxResult.Sent)
              case _                  => IO.pure(OutboxResult.Skip("宛先がありません"))
          }
    }

/** タスクの操作（REST・MCP・自動化で共通）。権限の確認・変更履歴・通知・Webhook・自動化の起動まで行う。 */
final class TaskService(tasks: TaskStore, projects: ProjectStore, access: Access2, hub: EventHub, notifier: NotifyService, adminGroups: Set[String]):

  private def link(key: String) = s"/tasks/$key"

  private def needWrite(op: Op): Either[Fail, Unit] =
    Either.cond(op.s.canWrite, (), Fail.Forbidden("このAPIキーは参照専用です"))

  // ---------------------------------------------------------------- 作成

  /** タスクを作る。notify が false なら担当・メンションの通知を出さない（取り込み）。 */
  def create(op: Op, projectId: UUID, r: CreateTaskReq, notify: Boolean = true): Res[TaskDto] =
    (needWrite(op), Dates.parse(r.startDate, "開始日"), Dates.parse(r.dueDate, "期日")).tupled match
      case Left(f) => IO.pure(Left(f))
      case Right(_) if r.title.trim.isEmpty => IO.pure(Left(Fail.Invalid("タイトルを入力してください")))
      case Right(_) if r.priority.exists(p => !Priority.all.contains(p)) || r.taskType.exists(t => !TaskType.all.contains(t)) =>
        IO.pure(Left(Fail.Invalid(s"priority は ${Priority.all.mkString(" / ")}、taskType は ${TaskType.all.mkString(" / ")} です")))
      case Right((_, start, due)) if start.zip(due).exists((s, d) => s.isAfter(d)) => IO.pure(Left(Fail.Invalid("開始日は期日より前にしてください")))
      case Right((_, start, due)) =>
        access.project(op.s, projectId, Role.Edit).flatMap {
          case Left(f) => IO.pure(Left(f))
          case Right(_) =>
            val desc     = r.description.filter(d => RichText.valid(d) && !RichText.isBlank(d))
            val descText = desc.fold("")(RichText.toPlain)
            tasks.create(projectId, r.copy(description = desc), descText, Some(r.reporterId.getOrElse(op.userId)), start, due).orFail.flatMap {
              case Left(f) => IO.pure(Left(f))
              case Right(t) =>
                for
                  _ <- hub.emit(op, projectId, Some(t.id), EventKind.TaskCreated, Json.obj("title" -> t.title.asJson, "key" -> t.key.asJson))
                  _ <- t.assigneeId.filterNot(_ == op.userId).filter(_ => notify).traverse_(a =>
                         notifier.send(NotificationKind.Assigned, Set(a), Some(op.userId), projectId, Some(t.id), None,
                           s"${t.key} の担当になりました", t.title, link(t.key)))
                  _ <- desc.filter(_ => notify).traverse_(d => mentioned(projectId, d).flatMap(m => mention(op, t, None, m, "説明")))
                  dto <- tasks.dto(t.id)
                yield dto.toRight(Fail.NotFound("タスクが見つかりません"))
            }
        }

  // ---------------------------------------------------------------- 更新

  /** JSON の更新内容（TaskPatch のキーだけ）を TaskChanges にする。 */
  def parsePatch(j: Json): Either[Fail, TaskChanges] =
    val c = j.hcursor
    def has(k: String) = c.downField(k).succeeded
    def optUuid(k: String): Either[Fail, Option[Option[UUID]]] =
      if !has(k) then Right(None)
      else c.downField(k).focus.filterNot(_.isNull) match
        case None    => Right(Some(None))
        case Some(v) => v.asString.flatMap(uuidOpt).toRight(Fail.Invalid(s"$k が UUID ではありません")).map(u => Some(Some(u)))
    def optDate(k: String, what: String): Either[Fail, Option[Option[LocalDate]]] =
      if !has(k) then Right(None)
      else Dates.parse(c.get[Option[String]](k).toOption.flatten, what).map(Some(_))
    for
      title <- if has("title") then c.get[String]("title").left.map(_ => Fail.Invalid("title は文字列です")).flatMap(t =>
                 if t.trim.isEmpty then Left(Fail.Invalid("タイトルを入力してください")) else Right(Some(t))) else Right(None)
      desc <- parseDescription(c.downField("description").focus, has("description"))
      status   <- optUuid("statusId").flatMap {
                    case Some(None) => Left(Fail.Invalid("ステータスは空にできません"))
                    case other      => Right(other.flatten)
                  }
      sprint   <- optUuid("sprintId")
      assignee <- optUuid("assigneeId")
      reporter <- optUuid("reporterId")
      parent   <- optUuid("parentId")
      start    <- optDate("startDate", "開始日")
      due      <- optDate("dueDate", "期日")
      priority <- if !has("priority") then Right(None)
                  else c.get[String]("priority").toOption.flatMap(Priority.parse).map(Some(_))
                    .toRight(Fail.Invalid(s"priority は ${Priority.all.mkString(" / ")} です"))
      taskType <- if !has("taskType") then Right(None)
                  else c.get[String]("taskType").toOption.flatMap(TaskType.parse).map(Some(_))
                    .toRight(Fail.Invalid(s"taskType は ${TaskType.all.mkString(" / ")} です"))
      labels   <- if !has("labelIds") then Right(None)
                  else c.get[List[String]]("labelIds").left.map(_ => Fail.Invalid("labelIds は配列です"))
                    .flatMap(_.traverse(s => uuidOpt(s).toRight(Fail.Invalid(s"ラベル $s が UUID ではありません")))).map(Some(_))
      fields   <- if !has("fields") then Right(Map.empty[UUID, Option[Json]])
                  else c.downField("fields").as[Map[String, Json]].left.map(_ => Fail.Invalid("fields はオブジェクトです"))
                    .flatMap(_.toList.traverse((k, v) => uuidOpt(k).toRight(Fail.Invalid(s"フィールド $k が UUID ではありません")).map(_ -> Option(v).filterNot(_.isNull))))
                    .map(_.toMap)
      version  = c.get[Int]("version").toOption
    yield TaskChanges(
      title = title,
      description = desc.map(_.map(d => d -> RichText.toPlain(d))),
      statusId = status,
      sprintId = sprint,
      assigneeId = assignee,
      reporterId = reporter,
      parentId = parent,
      priority = priority,
      taskType = taskType,
      startDate = start,
      dueDate = due,
      labelIds = labels,
      fields = fields,
      expectedVersion = version
    )

  private def parseDescription(v: Option[Json], present: Boolean): Either[Fail, Option[Option[Json]]] =
    if !present then Right(None)
    else v.filterNot(_.isNull) match
      case None => Right(Some(None))
      case Some(d) if d.isString => Right(Some(Some(RichText.fromMarkdown(d.asString.get)).filterNot(RichText.isBlank)))
      case Some(d) if RichText.valid(d) => Right(Some(Some(d).filterNot(RichText.isBlank)))
      case _ => Left(Fail.Invalid("description は Tiptap の文書（type: doc）か文字列です"))

  def update(op: Op, task: TaskCore, ch: TaskChanges): Res[TaskDto] =
    needWrite(op) match
      case Left(f) => IO.pure(Left(f))
      case Right(_) => tasks.update(task.id, ch).orFail.flatMap {
        case Left(f) => IO.pure(Left(f))
        case Right(r) => afterUpdate(op, r).flatMap(_ => tasks.dto(task.id).map(_.toRight(Fail.NotFound("タスクが見つかりません"))))
      }

  def move(op: Op, task: TaskCore, r: MoveTaskReq): Res[TaskDto] =
    needWrite(op) match
      case Left(f) => IO.pure(Left(f))
      case Right(_) => tasks.move(task.id, r).orFail.flatMap {
        case Left(f) => IO.pure(Left(f))
        case Right(u) => afterUpdate(op, u).flatMap(_ => tasks.dto(task.id).map(_.toRight(Fail.NotFound("タスクが見つかりません"))))
      }

  /** 変更履歴・通知・Webhook・自動化。並び順だけの変更は履歴に残さない。 */
  private def afterUpdate(op: Op, r: UpdateResult): IO[Unit] =
    val a = r.after
    if r.changes.isEmpty then
      // 並び順だけが変わった（他の人の画面のカンバンを更新するため、軽い履歴を残す）
      IO.whenA(r.before.rank != a.rank)(tasks.event(a.projectId, Some(a.id), Some(op.userId), op.via, "task.ranked", Json.obj()).void)
    else
      val detail = Json.obj("changes" -> Json.fromValues(r.changes.map(c => Json.obj("field" -> c.field.asJson, "from" -> c.from, "to" -> c.to))))
      for
        statuses <- projects.statuses(a.projectId)
        sname = (id: UUID) => statuses.find(_.id == id).map(_.name).getOrElse("")
        _ <- hub.emit(op, a.projectId, Some(a.id), EventKind.TaskUpdated, detail)
        statusChanged = r.before.statusId != a.statusId
        _ <- IO.whenA(statusChanged)(hub.emit(op, a.projectId, Some(a.id), EventKind.StatusChanged,
               Json.obj("from" -> sname(r.before.statusId).asJson, "to" -> sname(a.statusId).asJson,
                 "fromCategory" -> statuses.find(_.id == r.before.statusId).map(_.category).asJson,
                 "toCategory" -> statuses.find(_.id == a.statusId).map(_.category).asJson)).void)
        watchers <- tasks.watchers(a.id)
        _ <- a.assigneeId.filter(_ => r.before.assigneeId != a.assigneeId).traverse_ { u =>
               tasks.watch(a.id, u) *> notifier.send(NotificationKind.Assigned, Set(u), Some(op.userId), a.projectId, Some(a.id), None,
                 s"${a.key} の担当になりました", a.title, link(a.key))
             }
        names = r.changes.map(c => TaskPatch.label(c.field)).distinct.mkString("・")
        _ <- (if statusChanged then
                notifier.send(NotificationKind.Status, watchers.toSet, Some(op.userId), a.projectId, Some(a.id), None,
                  s"${a.key} が「${sname(a.statusId)}」になりました", a.title, link(a.key), mailDelay = 60, dedupe = Some(s"task:${a.id}"))
              else notifier.send(NotificationKind.Updated, watchers.toSet -- a.assigneeId.filter(_ => r.before.assigneeId != a.assigneeId).toSet,
                  Some(op.userId), a.projectId, Some(a.id), None, s"${a.key} が更新されました（$names）", a.title, link(a.key),
                  mailDelay = 300, dedupe = Some(s"task:${a.id}")))
        _ <- r.changes.find(_.field == "description").traverse_ { _ =>
               (a.description.fold(IO.pure(Set.empty[UUID]))(mentioned(a.projectId, _)),
                 r.before.description.fold(IO.pure(Set.empty[UUID]))(mentioned(a.projectId, _))).tupled.flatMap((newM, oldM) =>
                 mention(op, a, None, newM -- oldM, "説明"))
             }
      yield ()

  /** メンションされた人（グループ・役割へのメンションは、そのプロジェクトを閲覧できる所属者に広げる）。 */
  private def mentioned(projectId: UUID, doc: Json): IO[Set[UUID]] =
    RichText.groupMentions(doc).toList.flatTraverse { k =>
      if k.startsWith("g:") then uuidOpt(k.drop(2)).fold(IO.pure(List.empty[UUID]))(g => projects.groupMembers(projectId, g, adminGroups))
      else projects.usersWithRole(projectId, k.drop(2))
    }.map(_.toSet ++ RichText.mentions(doc))

  private def mention(op: Op, t: TaskCore, commentId: Option[UUID], ids: Set[UUID], where: String): IO[Unit] =
    IO.whenA(ids.nonEmpty)(
      notifier.send(NotificationKind.Mention, ids, Some(op.userId), t.projectId, Some(t.id), commentId,
        s"${t.key} の${where}でメンションされました", t.title, link(t.key) + commentId.fold("")(c => s"#comment-$c"))
    )

  def delete(op: Op, task: TaskCore): Res[Unit] =
    needWrite(op) match
      case Left(f) => IO.pure(Left(f))
      case Right(_) =>
        tasks.delete(task.id).flatMap { ids =>
          hub.emit(op, task.projectId, None, EventKind.TaskDeleted,
            Json.obj("taskId" -> task.id.asJson, "key" -> task.key.asJson, "title" -> task.title.asJson, "deleted" -> ids.asJson)).as(Right(()))
        }

  // ---------------------------------------------------------------- コメント

  def addComment(op: Op, task: TaskCore, body: Json): Res[CommentDto] =
    val doc = if body.isString then RichText.fromMarkdown(body.asString.get) else body
    if !RichText.valid(doc) || RichText.isBlank(doc) then IO.pure(Left(Fail.Invalid("コメントを入力してください")))
    else if !op.s.canWrite then IO.pure(Left(Fail.Forbidden("このAPIキーは参照専用です")))
    else
      for
        c <- tasks.addComment(task.id, Some(op.userId), doc, RichText.toPlain(doc), op.via)
        _ <- hub.emit(op, task.projectId, Some(task.id), EventKind.CommentAdded,
               Json.obj("commentId" -> c.id.asJson, "text" -> RichText.toPlain(doc).take(500).asJson))
        _ <- tasks.watch(task.id, op.userId).whenA(op.s.isSession)
        watchers <- tasks.watchers(task.id)
        ms <- mentioned(task.projectId, doc)
        _ <- mention(op, task, Some(c.id), ms, "コメント")
        _ <- notifier.send(NotificationKind.Comment, watchers.toSet -- ms, Some(op.userId), task.projectId, Some(task.id), Some(c.id),
               s"${task.key} にコメントがありました", RichText.toPlain(doc).take(300), link(task.key) + s"#comment-${c.id}", mailDelay = 60)
      yield Right(c)

  def updateComment(op: Op, task: TaskCore, commentId: UUID, body: Json): Res[CommentDto] =
    val doc = if body.isString then RichText.fromMarkdown(body.asString.get) else body
    tasks.comment(commentId).flatMap {
      case None => IO.pure(Left(Fail.NotFound("コメントが見つかりません")))
      case Some(old) if !old.author.exists(_.id == op.userId) && !op.s.isAdmin => IO.pure(Left(Fail.Forbidden("自分のコメントだけ編集できます")))
      case Some(_) if !RichText.valid(doc) || RichText.isBlank(doc) => IO.pure(Left(Fail.Invalid("コメントを入力してください")))
      case Some(old) =>
        for
          _ <- tasks.updateComment(commentId, doc, RichText.toPlain(doc))
          _ <- hub.emit(op, task.projectId, Some(task.id), EventKind.CommentUpdated, Json.obj("commentId" -> commentId.asJson))
          now    <- mentioned(task.projectId, doc)
          before <- mentioned(task.projectId, old.body)
          _ <- mention(op, task, Some(commentId), now -- before, "コメント")
          c <- tasks.comment(commentId)
        yield c.toRight(Fail.NotFound("コメントが見つかりません"))
    }

  def deleteComment(op: Op, task: TaskCore, commentId: UUID, role: Role): Res[Unit] =
    tasks.comment(commentId).flatMap {
      case None => IO.pure(Left(Fail.NotFound("コメントが見つかりません")))
      case Some(old) if !old.author.exists(_.id == op.userId) && role < Role.Admin => IO.pure(Left(Fail.Forbidden("自分のコメントだけ削除できます")))
      case Some(_) =>
        tasks.deleteComment(commentId) *>
          hub.emit(op, task.projectId, Some(task.id), EventKind.CommentDeleted, Json.obj("commentId" -> commentId.asJson)).as(Right(()))
    }

  // ---------------------------------------------------------------- 作業時間

  def addWorklog(op: Op, task: TaskCore, r: WorklogReq): Res[WorklogDto] =
    Dates.parse(Some(r.workDate), "作業日") match
      case Left(f) => IO.pure(Left(f))
      case Right(None) => IO.pure(Left(Fail.Invalid("作業日を指定してください")))
      case Right(Some(_)) if r.minutes <= 0 || r.minutes > 24 * 60 => IO.pure(Left(Fail.Invalid("作業時間は1分〜24時間で指定してください")))
      case Right(Some(d)) =>
        for
          w <- tasks.addWorklog(task.id, op.userId, d, r.minutes, r.note.getOrElse("").trim)
          _ <- hub.emit(op, task.projectId, Some(task.id), EventKind.WorklogAdded,
                 Json.obj("worklogId" -> w.id.asJson, "minutes" -> r.minutes.asJson, "workDate" -> d.toString.asJson))
        yield Right(w)

  // ---------------------------------------------------------------- 一括

  def bulk(op: Op, ids: List[UUID], patch: Json): Res[Int] =
    parsePatch(patch) match
      case Left(f) => IO.pure(Left(f))
      case Right(ch) =>
        ids.distinct.traverse { id =>
          access.task(op.s, id.toString, Role.Edit).flatMap {
            case Left(_)       => IO.pure(0)
            case Right((t, _)) => update(op, t, ch.copy(expectedVersion = None)).map(_.fold(_ => 0, _ => 1))
          }
        }.map(n => Right(n.sum))
