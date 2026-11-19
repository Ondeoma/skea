package skea.api

import sbirds.shared.AppError

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import skea.core.*
import skea.db.*
import skea.shared.*

import java.time.LocalDate
import java.util.UUID

/** タスクの操作（REST・MCP・自動化で共通）。権限の確認・変更履歴・通知・Webhook・自動化の起動まで行う。 */
final class TaskService(tasks: TaskStore,
                        projects: ProjectStore,
                        access: Access2,
                        hub: EventHub,
                        notifier: NotifyService,
                        adminGroups: Set[String]) {

  private def link(key: String) = s"/tasks/$key"

  /** 担当者・報告者は、そのプロジェクトを見られる人だけ。 */
  def peopleOk(projectId: UUID, ids: List[UUID]): IO[Either[AppError, Unit]] =
    if ids.isEmpty then IO.pure(Right(()))
    else
      projects.audience(projectId, adminGroups).map { a =>
        val members = a.map(_.id).toSet
        Either.cond(ids.forall(members), (), AppError(ErrorCode.TaskAssigneeNotMember))
      }

  private def needWrite(op: Op): Either[AppError, Unit] =
    Either.cond(op.s.canWrite, (), AppError(ErrorCode.ApiKeyReadOnly))

  // ---------------------------------------------------------------- 作成

  /** タスクを作る。notify が false なら担当・メンションの通知を出さない（取り込み）。need は必要な権限（ヘルプデスクの問い合わせは Requester）。 */
  def create(op: Op, projectId: UUID, r: CreateTaskReq, notify: Boolean = true, need: Role = Role.Edit): Res[TaskDto] =
    sbirds.observe.Telemetry.span(
      "task.create",
      "project.id" -> projectId.toString
    )(createImpl(op, projectId, r, notify, need))
      .flatTap(res => IO.whenA(res.isRight)(sbirds.observe.Metrics.count("skea.tasks.created", "via" -> op.via)))

  private def createImpl(op: Op, projectId: UUID, r: CreateTaskReq, notify: Boolean, need: Role): Res[TaskDto] =
    (needWrite(op), Dates.parse(r.startDate, "開始日"), Dates.parse(r.dueDate, "期日")).tupled match {
      case Left(f) => IO.pure(Left(f))
      case Right(_) if r.title.trim.isEmpty => IO.pure(Left(AppError(ErrorCode.TaskTitleRequired)))
      case Right(_)
          if r.priority.exists(p => !Priority.all.contains(p)) || r.taskType.exists(t => !TaskType.all.contains(t)) =>
        IO.pure(Left(
          AppError(
            ErrorCode.TaskPriorityOrTypeInvalid,
            s"priority は ${Priority.all.mkString(" / ")}、taskType は ${TaskType.all.mkString(" / ")} です"
          )
        ))
      case Right((_, start, due))
          if start.zip(due).exists((s, d) => s.isAfter(d)) => IO.pure(Left(AppError(ErrorCode.TaskStartAfterDue)))
      case Right((_, start, due)) =>
        access.project(op.s, projectId, need).flatMap {
          case Left(f) => IO.pure(Left(f): Either[AppError, Unit])
          case Right(_) => peopleOk(projectId, r.assigneeId.toList ++ r.reporterId.toList)
        }.flatMap {
          case Left(f) => IO.pure(Left(f))
          case Right(_) =>
            val desc = r.description.filter(d => RichText.valid(d) && !RichText.isBlank(d))
            val descText = desc.fold("")(RichText.toPlain)
            tasks.create(
              projectId,
              r.copy(description = desc),
              descText,
              Some(r.reporterId.getOrElse(op.userId)),
              start,
              due
            ).flatMap {
              case Left(f) => IO.pure(Left(f))
              case Right(t) =>
                for {
                  _ <- hub.emit(
                    op,
                    projectId,
                    Some(t.id),
                    EventKind.TaskCreated,
                    Json.obj("title" -> t.title.asJson, "key" -> t.key.asJson)
                  )
                  _ <- t.assigneeId.filterNot(_ == op.userId).filter(_ => notify).traverse_(a =>
                    notifier.send(
                      NotificationKind.Assigned,
                      Set(a),
                      Some(op.userId),
                      projectId,
                      Some(t.id),
                      None,
                      s"${t.key} の担当になりました",
                      t.title,
                      link(t.key)
                    )
                  )
                  _ <- desc.filter(_ => notify).traverse_(d =>
                    mentioned(projectId, d).flatMap(m => mention(op, t, None, m, "説明"))
                  )
                  dto <- tasks.dto(t.id)
                } yield dto.toRight(AppError(ErrorCode.TaskNotFound))
            }
        }
    }

  // ---------------------------------------------------------------- 更新

  /** JSON の更新内容（TaskPatch のキーだけ）を TaskChanges にする。 */
  def parsePatch(j: Json): Either[AppError, TaskChanges] = {
    val c = j.hcursor
    def has(k: String) = c.downField(k).succeeded
    def optUuid(k: String): Either[AppError, Option[Option[UUID]]] =
      if !has(k) then Right(None)
      else
        c.downField(k).focus.filterNot(_.isNull) match {
          case None => Right(Some(None))
          case Some(v) =>
            v.asString.flatMap(uuidOpt).toRight(AppError(ErrorCode.TaskPatchIdInvalid, s"$k が UUID ではありません")).map(u =>
              Some(Some(u))
            )
        }
    def optDate(k: String, what: String): Either[AppError, Option[Option[LocalDate]]] =
      if !has(k) then Right(None)
      else Dates.parse(c.get[Option[String]](k).toOption.flatten, what).map(Some(_))
    for {
      title <- if has("title") then
        c.get[String]("title").left.map(_ => AppError(ErrorCode.TaskPatchTitleNotString)).flatMap(t =>
          if t.trim.isEmpty then Left(AppError(ErrorCode.TaskTitleRequired)) else Right(Some(t))
        )
      else Right(None)
      desc <- parseDescription(c.downField("description").focus, has("description"))
      status <- optUuid("statusId").flatMap {
        case Some(None) => Left(AppError(ErrorCode.TaskStatusRequired))
        case other => Right(other.flatten)
      }
      sprint <- optUuid("sprintId")
      assignee <- optUuid("assigneeId")
      reporter <- optUuid("reporterId")
      parent <- optUuid("parentId")
      start <- optDate("startDate", "開始日")
      due <- optDate("dueDate", "期日")
      priority <- if !has("priority") then Right(None)
      else
        c.get[String]("priority").toOption.flatMap(Priority.parse).map(Some(_))
          .toRight(AppError(ErrorCode.TaskPriorityInvalid, s"priority は ${Priority.all.mkString(" / ")} です"))
      taskType <- if !has("taskType") then Right(None)
      else
        c.get[String]("taskType").toOption.flatMap(TaskType.parse).map(Some(_))
          .toRight(AppError(ErrorCode.TaskTypeInvalid, s"taskType は ${TaskType.all.mkString(" / ")} です"))
      labels <- if !has("labelIds") then Right(None)
      else
        c.get[List[String]]("labelIds").left.map(_ => AppError(ErrorCode.TaskLabelIdsNotArray))
          .flatMap(_.traverse(s =>
            uuidOpt(s).toRight(AppError(ErrorCode.TaskLabelIdInvalid, s"ラベル $s が UUID ではありません"))
          )).map(Some(_))
      fields <- if !has("fields") then Right(Map.empty[UUID, Option[Json]])
      else
        c.downField("fields").as[Map[String, Json]].left.map(_ => AppError(ErrorCode.TaskFieldsNotObject))
          .flatMap(_.toList.traverse((k, v) =>
            uuidOpt(k).toRight(AppError(ErrorCode.TaskFieldIdInvalid, s"フィールド $k が UUID ではありません")).map(_ ->
              Option(v).filterNot(_.isNull))
          ))
          .map(_.toMap)
      version = c.get[Int]("version").toOption
    } yield TaskChanges(
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
  }

  private def parseDescription(v: Option[Json], present: Boolean): Either[AppError, Option[Option[Json]]] =
    if !present then Right(None)
    else
      v.filterNot(_.isNull) match {
        case None => Right(Some(None))
        case Some(d) if d.isString =>
          Right(Some(Some(RichText.fromMarkdown(d.asString.get)).filterNot(RichText.isBlank)))
        case Some(d) if RichText.valid(d) => Right(Some(Some(d).filterNot(RichText.isBlank)))
        case _ => Left(AppError(ErrorCode.TaskDescriptionInvalid))
      }

  def update(op: Op, task: TaskCore, ch: TaskChanges): Res[TaskDto] =
    needWrite(op) match {
      case Left(f) => IO.pure(Left(f))
      case Right(_) => peopleOk(
          task.projectId,
          ch.assigneeId.flatten.filterNot(task.assigneeId.contains).toList ++
            ch.reporterId.flatten.filterNot(task.reporterId.contains).toList
        ).flatMap {
          case Left(f) => IO.pure(Left(f))
          case Right(_) => tasks.update(task.id, ch)
        }.flatMap {
          case Left(f) => IO.pure(Left(f))
          case Right(r) =>
            afterUpdate(op, r).flatMap(_ => tasks.dto(task.id).map(_.toRight(AppError(ErrorCode.TaskNotFound))))
        }
    }

  def move(op: Op, task: TaskCore, r: MoveTaskReq): Res[TaskDto] =
    needWrite(op) match {
      case Left(f) => IO.pure(Left(f))
      case Right(_) => tasks.move(task.id, r).flatMap {
          case Left(f) => IO.pure(Left(f))
          case Right(u) =>
            afterUpdate(op, u).flatMap(_ => tasks.dto(task.id).map(_.toRight(AppError(ErrorCode.TaskNotFound))))
        }
    }

  /** 変更履歴・通知・Webhook・自動化。並び順だけの変更は履歴に残さない。 */
  private def afterUpdate(op: Op, r: UpdateResult): IO[Unit] = {
    val a = r.after
    if r.changes.isEmpty then
      // 並び順だけが変わった（他の人の画面のカンバンを更新するため、軽い履歴を残す）
      IO.whenA(r.before.rank != a.rank)(tasks.event(
        a.projectId,
        Some(a.id),
        Some(op.userId),
        op.via,
        "task.ranked",
        Json.obj()
      ).void)
    else {
      val detail = Json.obj("changes" ->
        Json.fromValues(r.changes.map(c => Json.obj("field" -> c.field.asJson, "from" -> c.from, "to" -> c.to))))
      for {
        statuses <- projects.statuses(a.projectId)
        sname = (id: UUID) => statuses.find(_.id == id).map(_.name).getOrElse("")
        _ <- hub.emit(op, a.projectId, Some(a.id), EventKind.TaskUpdated, detail)
        statusChanged = r.before.statusId != a.statusId
        toCategory = statuses.find(_.id == a.statusId).map(_.category)
        _ <- IO.whenA(statusChanged)(hub.emit(
          op,
          a.projectId,
          Some(a.id),
          EventKind.StatusChanged,
          Json.obj(
            "from" -> sname(r.before.statusId).asJson,
            "to" -> sname(a.statusId).asJson,
            "fromCategory" -> statuses.find(_.id == r.before.statusId).map(_.category).asJson,
            "toCategory" -> toCategory.asJson
          )
        ).void *>
          sbirds.observe.Metrics.count(
            "skea.tasks.status_changed",
            "to" -> toCategory.getOrElse(""),
            "via" -> op.via
          ) *>
          IO.whenA(toCategory.contains("done"))(sbirds.observe.Metrics.count("skea.tasks.completed", "via" -> op.via)))
        watchers <- tasks.watchers(a.id)
        _ <- a.assigneeId.filter(_ => r.before.assigneeId != a.assigneeId).traverse_ { u =>
          tasks.watch(a.id, u) *> notifier.send(
            NotificationKind.Assigned,
            Set(u),
            Some(op.userId),
            a.projectId,
            Some(a.id),
            None,
            s"${a.key} の担当になりました",
            a.title,
            link(a.key)
          )
        }
        names = r.changes.map(c => TaskPatch.label(c.field)).distinct.mkString("・")
        _ <- (if statusChanged then
                notifier.send(
                  NotificationKind.Status,
                  watchers.toSet,
                  Some(op.userId),
                  a.projectId,
                  Some(a.id),
                  None,
                  s"${a.key} が「${sname(a.statusId)}」になりました",
                  a.title,
                  link(a.key),
                  mailDelay = 60,
                  dedupe = Some(s"task:${a.id}")
                )
              else
                notifier.send(
                  NotificationKind.Updated,
                  watchers.toSet -- a.assigneeId.filter(_ => r.before.assigneeId != a.assigneeId).toSet,
                  Some(op.userId),
                  a.projectId,
                  Some(a.id),
                  None,
                  s"${a.key} が更新されました（$names）",
                  a.title,
                  link(a.key),
                  mailDelay = 300,
                  dedupe = Some(s"task:${a.id}")
                )
        )
        _ <- r.changes.find(_.field == "description").traverse_ { _ =>
          (
            a.description.fold(IO.pure(Set.empty[UUID]))(mentioned(a.projectId, _)),
            r.before.description.fold(IO.pure(Set.empty[UUID]))(mentioned(a.projectId, _))
          ).tupled.flatMap((newM, oldM) =>
            mention(op, a, None, newM -- oldM, "説明")
          )
        }
      } yield ()
    }
  }

  /** メンションされた人（グループ・役割へのメンションは、そのプロジェクトを閲覧できる所属者に広げる）。 */
  private def mentioned(projectId: UUID, doc: Json): IO[Set[UUID]] =
    RichText.groupMentions(doc).toList.flatTraverse { k =>
      if k.startsWith("g:") then
        uuidOpt(k.drop(2)).fold(IO.pure(List.empty[UUID]))(g => projects.groupMembers(projectId, g, adminGroups))
      else projects.usersWithRole(projectId, k.drop(2))
    }.map(_.toSet ++ RichText.mentions(doc))

  private def mention(op: Op,
                      t: TaskCore,
                      commentId: Option[UUID],
                      ids: Set[UUID],
                      where: String,
                      internal: Boolean = false): IO[Unit] =
    IO.whenA(ids.nonEmpty)(
      notifier.send(
        NotificationKind.Mention,
        ids,
        Some(op.userId),
        t.projectId,
        Some(t.id),
        commentId,
        s"${t.key} の${where}でメンションされました",
        t.title,
        link(t.key) + commentId.fold("")(c => s"#comment-$c"),
        internal = internal
      )
    )

  def delete(op: Op, task: TaskCore): Res[Unit] =
    needWrite(op) match {
      case Left(f) => IO.pure(Left(f))
      case Right(_) =>
        tasks.delete(task.id).flatMap { ids =>
          hub.emit(
            op,
            task.projectId,
            None,
            EventKind.TaskDeleted,
            Json.obj(
              "taskId" -> task.id.asJson,
              "key" -> task.key.asJson,
              "title" -> task.title.asJson,
              "deleted" -> ids.asJson
            )
          ).as(Right(()))
        }
    }

  // ---------------------------------------------------------------- コメント

  /** コメントを追加する。internal は内部コメント（ヘルプデスクのエージェントだけに見え、"ユーザ"には通知しない）。 */
  def addComment(op: Op, task: TaskCore, body: Json, internal: Boolean = false): Res[CommentDto] = {
    val doc = if body.isString then RichText.fromMarkdown(body.asString.get) else body
    if !RichText.valid(doc) || RichText.isBlank(doc) then IO.pure(Left(AppError(ErrorCode.CommentRequired)))
    else if !op.s.canWrite then IO.pure(Left(AppError(ErrorCode.ApiKeyReadOnly)))
    else
      for {
        c <- sbirds.observe.Telemetry.span(
          "comment.add",
          "task.id" -> task.id.toString
        )(tasks.addComment(task.id, Some(op.userId), doc, RichText.toPlain(doc), op.via, internal))
        _ <- sbirds.observe.Metrics.count("skea.comments.added", "via" -> op.via, "internal" -> internal.toString)
        _ <- hub.emit(
          op,
          task.projectId,
          Some(task.id),
          EventKind.CommentAdded,
          Json.obj(
            "commentId" -> c.id.asJson,
            "text" -> RichText.toPlain(doc).take(500).asJson,
            "internal" -> internal.asJson
          )
        )
        _ <- tasks.watch(task.id, op.userId).whenA(op.s.isSession)
        watchers <- tasks.watchers(task.id)
        ms <- mentioned(task.projectId, doc)
        _ <- mention(op, task, Some(c.id), ms, if internal then "内部コメント" else "コメント", internal)
        _ <- notifier.send(
          NotificationKind.Comment,
          watchers.toSet -- ms,
          Some(op.userId),
          task.projectId,
          Some(task.id),
          Some(c.id),
          s"${task.key} に${if internal then "内部コメント" else "コメント"}がありました",
          RichText.toPlain(doc).take(300),
          link(task.key) + s"#comment-${c.id}",
          mailDelay = 60,
          internal = internal
        )
      } yield Right(c)
  }

  def updateComment(op: Op, task: TaskCore, commentId: UUID, body: Json): Res[CommentDto] = {
    val doc = if body.isString then RichText.fromMarkdown(body.asString.get) else body
    tasks.comment(commentId).flatMap {
      case None => IO.pure(Left(AppError(ErrorCode.CommentNotFound)))
      case Some(old) if !old.author.exists(_.id == op.userId) && !op.s.canAdmin =>
        IO.pure(Left(AppError(ErrorCode.CommentEditNotOwner)))
      case Some(_) if !RichText.valid(doc) || RichText.isBlank(doc) =>
        IO.pure(Left(AppError(ErrorCode.CommentRequired)))
      case Some(old) =>
        for {
          _ <- tasks.updateComment(commentId, doc, RichText.toPlain(doc))
          _ <- hub.emit(
            op,
            task.projectId,
            Some(task.id),
            EventKind.CommentUpdated,
            Json.obj("commentId" -> commentId.asJson)
          )
          now <- mentioned(task.projectId, doc)
          before <- mentioned(task.projectId, old.body)
          _ <- mention(op, task, Some(commentId), now -- before, "コメント", old.internal)
          c <- tasks.comment(commentId)
        } yield c.toRight(AppError(ErrorCode.CommentNotFound))
    }
  }

  def deleteComment(op: Op, task: TaskCore, commentId: UUID, role: Role): Res[Unit] =
    tasks.comment(commentId).flatMap {
      case None => IO.pure(Left(AppError(ErrorCode.CommentNotFound)))
      case Some(old) if !old.author.exists(_.id == op.userId) && role < Role.Admin =>
        IO.pure(Left(AppError(ErrorCode.CommentDeleteNotOwner)))
      case Some(_) =>
        tasks.deleteComment(commentId) *>
          hub.emit(
            op,
            task.projectId,
            Some(task.id),
            EventKind.CommentDeleted,
            Json.obj("commentId" -> commentId.asJson)
          ).as(Right(()))
    }

  // ---------------------------------------------------------------- 作業時間

  def addWorklog(op: Op, task: TaskCore, r: WorklogReq): Res[WorklogDto] =
    Dates.parse(Some(r.workDate), "作業日") match {
      case Left(f) => IO.pure(Left(f))
      case Right(None) => IO.pure(Left(AppError(ErrorCode.WorklogDateRequired)))
      case Right(Some(_)) if r.minutes <= 0 || r.minutes > 24 * 60 =>
        IO.pure(Left(AppError(ErrorCode.WorklogMinutesInvalid)))
      case Right(Some(d)) =>
        for {
          w <- tasks.addWorklog(task.id, op.userId, d, r.minutes, r.note.getOrElse("").trim)
          _ <- hub.emit(
            op,
            task.projectId,
            Some(task.id),
            EventKind.WorklogAdded,
            Json.obj("worklogId" -> w.id.asJson, "minutes" -> r.minutes.asJson, "workDate" -> d.toString.asJson)
          )
          _ <- sbirds.observe.Metrics.count("skea.worklogs.added", "via" -> op.via) *>
            sbirds.observe.Metrics.add("skea.worklogs.minutes", r.minutes.toLong)
        } yield Right(w)
    }

  // ---------------------------------------------------------------- 一括

  def bulk(op: Op, ids: List[UUID], patch: Json): Res[Int] =
    parsePatch(patch) match {
      case Left(f) => IO.pure(Left(f))
      case Right(ch) =>
        ids.distinct.traverse { id =>
          access.task(op.s, id.toString, Role.Edit).flatMap {
            case Left(_) => IO.pure(0)
            case Right((t, _)) => update(op, t, ch.copy(expectedVersion = None)).map(_.fold(_ => 0, _ => 1))
          }
        }.map(n => Right(n.sum))
    }
}
