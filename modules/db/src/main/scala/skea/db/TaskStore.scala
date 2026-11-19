package skea.db

import sbirds.shared.AppError

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.postgres.circe.jsonb.implicits.*
import io.circe.Json
import io.circe.syntax.*
import skea.core.*
import skea.shared.*

import java.time.{Instant, LocalDate}
import java.util.UUID

/** タスクの列そのもの（更新前後の比較・権限の確認に使う）。 */
final case class TaskCore(id: UUID,
                          projectId: UUID,
                          number: Long,
                          key: String,
                          parentId: Option[UUID],
                          title: String,
                          description: Option[Json],
                          statusId: UUID,
                          sprintId: Option[UUID],
                          assigneeId: Option[UUID],
                          reporterId: Option[UUID],
                          startDate: Option[LocalDate],
                          dueDate: Option[LocalDate],
                          rank: String,
                          version: Int,
                          resolvedAt: Option[Instant],
                          createdAt: Instant,
                          updatedAt: Instant,
                          priority: String,
                          taskType: String)

/** タスクの詳細の画面に出すもの一式。 */
final case class TaskDetailParts(task: TaskDto,
                                 core: TaskCore,
                                 ancestors: List[TaskRef],
                                 subtasks: List[TaskDto],
                                 watchers: List[UserBrief],
                                 links: List[TaskLinkDto],
                                 predecessors: List[TaskRef],
                                 successors: List[TaskRef],
                                 fields: Map[String, Json],
                                 commits: List[CommitDto],
                                 attachments: List[AttachmentDto],
                                 latestEventId: Long)

/** タスクの変更。None は「変えない」、Some(None) は「空にする」。 */
final case class TaskChanges(title: Option[String] = None,
                             description: Option[Option[(Json, String)]] = None,
                             statusId: Option[UUID] = None,
                             sprintId: Option[Option[UUID]] = None,
                             assigneeId: Option[Option[UUID]] = None,
                             reporterId: Option[Option[UUID]] = None,
                             parentId: Option[Option[UUID]] = None,
                             startDate: Option[Option[LocalDate]] = None,
                             dueDate: Option[Option[LocalDate]] = None,
                             labelIds: Option[List[UUID]] = None,
                             fields: Map[UUID, Option[Json]] = Map.empty,
                             rank: Option[String] = None,
                             expectedVersion: Option[Int] = None,
                             priority: Option[String] = None,
                             taskType: Option[String] = None) {
  def isEmpty: Boolean = this == TaskChanges(expectedVersion = expectedVersion)
}

/** 1つのフィールドの変更（変更履歴・Webhook・自動化に使う）。 */
final case class FieldChange(field: String, from: Json, to: Json)

final case class UpdateResult(before: TaskCore, after: TaskCore, changes: List[FieldChange])

/** タスクと、それに付くもの（ラベル・ウォッチャー・リンク・依存関係・作業時間・コメント・添付・コミット・変更履歴）。 */
final class TaskStore(xa: Transactor[IO]) extends TxStore(xa) {

  // ---------------------------------------------------------------- 読み取り

  private val coreCols =
    fr"""t.id, t.project_id, t.number, t.key, t.parent_id, t.title, t.description, t.status_id, t.sprint_id, t.assignee_id, t.reporter_id,
         t.start_date, t.due_date, t.rank, t.version, t.resolved_at, t.created_at, t.updated_at, t.priority, t.type"""

  def coreC(id: UUID): ConnectionIO[Option[TaskCore]] =
    (fr"select" ++ coreCols ++ fr"from task t where t.id = $id and t.deleted_at is null").query[TaskCore].option

  def core(id: UUID): IO[Option[TaskCore]] = tx(coreC(id))

  def coreByKey(key: String): IO[Option[TaskCore]] = tx(
    (fr"select" ++ coreCols ++ fr"from task t where t.key = ${key.trim.toUpperCase} and t.deleted_at is null").query[
      TaskCore
    ].option
  )

  /** TaskDto の列。 */
  private[db] val dtoSelect: Fragment =
    fr"""select t.id, t.key, t.project_id, t.parent_id, t.title, t.status_id, t.sprint_id,
           a.id, a.name, a.avatar_url, r.id, r.name, r.avatar_url,
           t.start_date::text, t.due_date::text,
           coalesce((select sum(w.minutes) from worklog w where w.task_id = t.id), 0)::int,
           t.rank,
           coalesce((select array_agg(tl.label_id) from task_label tl where tl.task_id = t.id), '{}'),
           sub.n, sub.done,
           (select count(*) from comment cm where cm.task_id = t.id and cm.deleted_at is null)::int,
           t.version, t.created_at, t.updated_at, t.resolved_at, t.priority, t.type
         from task t left join app_user a on a.id = t.assignee_id left join app_user r on r.id = t.reporter_id
         left join lateral (
           select count(*)::int as n, (count(*) filter (where cs.category = 'done'))::int as done
           from task c join status cs on cs.id = c.status_id where c.parent_id = t.id and c.deleted_at is null) sub on true"""

  def dtoC(id: UUID): ConnectionIO[Option[TaskDto]] =
    (dtoSelect ++ fr"where t.id = $id and t.deleted_at is null").query[TaskDto].option

  def dto(id: UUID): IO[Option[TaskDto]] = tx(dtoC(id))

  def dtos(ids: List[UUID]): IO[List[TaskDto]] =
    if ids.isEmpty then IO.pure(Nil)
    else
      tx((dtoSelect ++ fr"where t.id = any(${ids.toArray}) and t.deleted_at is null").query[TaskDto].to[List])
        .map { ts =>
          val m = ts.map(t => t.id -> t).toMap; ids.flatMap(m.get)
        }

  private val byRank = fr"""order by t.rank collate "C", t.number"""

  /** アクティブなスプリントのタスク（カンバン）。hideDoneAfterDays があれば、完了してその日数を過ぎたタスクを出さない（ヘルプデスク）。 */
  def board(projectId: UUID, sprintId: UUID, hideDoneAfterDays: Option[Int] = None): IO[List[TaskDto]] = tx(
    (dtoSelect ++ fr"where t.project_id = $projectId and t.sprint_id = $sprintId and t.deleted_at is null" ++
      hideDoneAfterDays.fold(Fragment.empty)(d =>
        fr"""and not (exists(select 1 from status s where s.id = t.status_id and s.category = 'done')
                      and coalesce(t.resolved_at, t.updated_at) < now() - make_interval(days => $d))"""
      ) ++ byRank).query[TaskDto].to[List]
  )

  /** バックログ: 完了していないスプリントのタスクと、スプリントのない未完了のタスク。 */
  def backlog(projectId: UUID): IO[List[TaskDto]] = tx(
    (dtoSelect ++ fr"""join status s on s.id = t.status_id left join sprint sp on sp.id = t.sprint_id
         where t.project_id = $projectId and t.deleted_at is null
           and ((t.sprint_id is not null and sp.state <> 'closed') or (t.sprint_id is null and s.category <> 'done'))""" ++
      byRank)
      .query[TaskDto].to[List]
  )

  /** プロジェクトのすべてのタスク（ガントチャート）と依存関係。 */
  def gantt(projectId: UUID, includeDone: Boolean): IO[GanttDto] = tx(
    for {
      ts <-
      (dtoSelect ++ fr"join status s on s.id = t.status_id where t.project_id = $projectId and t.deleted_at is null" ++
        (if includeDone then Fragment.empty else fr"and s.category <> 'done'") ++ byRank).query[TaskDto].to[List]
      ids = ts.map(_.id).toArray
      ds <- sql"""select predecessor_id, successor_id, kind from task_dependency
                  where predecessor_id = any($ids) and successor_id = any($ids)""".query[DependencyDto].to[List]
      ms <-
        sql"select id, name, date::text, color from milestone where project_id = $projectId order by date, name".query[
          MilestoneDto
        ].to[List]
    } yield GanttDto(ts, ds, ms)
  )

  def subtasksC(id: UUID): ConnectionIO[
    List[TaskDto]
  ] = (dtoSelect ++ fr"where t.parent_id = $id and t.deleted_at is null" ++ byRank).query[TaskDto].to[List]

  private def refC(where: Fragment): ConnectionIO[List[TaskRef]] =
    (fr"select t.id, t.key, t.title, s.name, s.category from task t join status s on s.id = t.status_id" ++
      where).query[TaskRef].to[List]

  /** 親から順の祖先（ルートが先頭）。 */
  def ancestorsC(id: UUID): ConnectionIO[List[TaskRef]] =
    sql"""with recursive up(id, parent_id, depth) as (
            select id, parent_id, 0 from task where id = $id
            union all select t.id, t.parent_id, up.depth + 1 from task t join up on t.id = up.parent_id where up.depth < 50)
          select t.id, t.key, t.title, s.name, s.category from up join task t on t.id = up.id join status s on s.id = t.status_id
          where up.depth > 0 order by up.depth desc""".query[TaskRef].to[List]

  /** 子孫（自分を含まない）。 */
  def descendantIdsC(id: UUID): ConnectionIO[List[UUID]] =
    sql"""with recursive down(id, depth) as (
            select id, 0 from task where parent_id = $id
            union all select t.id, d.depth + 1 from task t join down d on t.parent_id = d.id where d.depth < 50)
          select id from down""".query[UUID].to[List]

  /** タスクの詳細の画面に出すもの一式（1つのトランザクションで読む）。 */
  def detail(id: UUID): IO[Option[TaskDetailParts]] =
    tx(dtoC(id).flatMap(_.flatTraverse { t =>
      coreC(id).flatMap(_.traverse { c =>
        for {
          anc <- ancestorsC(id)
          subs <- subtasksC(id)
          ws <-
            sql"select u.id, u.name, u.avatar_url from task_watcher w join app_user u on u.id = w.user_id where w.task_id = $id order by u.name"
              .query[UserBrief].to[List]
          lks <- linksC(id)
          pre <- refC(
            fr"join task_dependency d on d.predecessor_id = t.id where d.successor_id = $id and t.deleted_at is null"
          )
          suc <- refC(
            fr"join task_dependency d on d.successor_id = t.id where d.predecessor_id = $id and t.deleted_at is null"
          )
          fs <-
            sql"select field_id::text, value from task_field_value where task_id = $id".query[(String, Json)].to[List]
          cms <- commitsC(id)
          ats <- attachmentsC(id)
          ev <- sql"select coalesce(max(id), 0) from task_event where project_id = ${t.projectId}".query[Long].unique
        } yield TaskDetailParts(t, c, anc, subs, ws, lks, pre, suc, fs.toMap, cms, ats, ev)
      })
    }))

  def links(id: UUID): IO[List[TaskLinkDto]] = tx(linksC(id))

  def attachments(id: UUID): IO[List[AttachmentDto]] = tx(attachmentsC(id))

  def attachmentsC(id: UUID): ConnectionIO[List[AttachmentDto]] =
    sql"""select a.id, a.file_name, a.size, a.content_type, u.id, u.name, u.avatar_url, a.created_at
          from attachment a left join app_user u on u.id = a.created_by where a.task_id = $id order by a.created_at"""
      .query[(UUID, String, Long, String, Option[UserBrief], Instant)]
      .map((aid, n, size, ct, by, at) => AttachmentDto(aid, n, size, ct, by, at, s"/api/attachments/$aid")).to[List]

  /** サブタスク・リンク先のタスク（自動化の for_each）。 */
  def subtaskIds(id: UUID): IO[List[UUID]] =
    tx(sql"select id from task where parent_id = $id and deleted_at is null order by rank collate \"C\"".query[
      UUID
    ].to[List])

  def linkedIds(id: UUID): IO[List[UUID]] = tx(linksC(id)).map(_.flatMap(_.target.map(_.id)))

  // ---------------------------------------------------------------- 作成

  /** タスクを作る。番号はプロジェクトの task_seq で採番する（行ロックで直列化）。 */
  def create(projectId: UUID,
             r: CreateTaskReq,
             descText: String,
             reporter: Option[UUID],
             start: Option[LocalDate],
             due: Option[LocalDate]): IO[Either[AppError, TaskCore]] = tx {
    val id = UUID.randomUUID()
    for {
      statusOk <- r.statusId.traverse(s =>
        sql"select exists(select 1 from status where id = $s and project_id = $projectId)".query[Boolean].unique
      )
      defaultStatus <- sql"""select id from status where project_id = $projectId
                             order by case category when 'todo' then 0 when 'in_progress' then 1 when 'waiting' then 2 else 3 end, position limit 1"""
        .query[UUID].option
      sprintOk <- r.sprintId.traverse(s =>
        sql"select exists(select 1 from sprint where id = $s and project_id = $projectId and state <> 'closed')".query[
          Boolean
        ].unique
      )
      parentOk <- r.parentId.traverse(p =>
        sql"select exists(select 1 from task where id = $p and project_id = $projectId and deleted_at is null)".query[
          Boolean
        ].unique
      )
      res <-
        if statusOk.contains(false) then fail(AppError(ErrorCode.TaskStatusNotInProject))
        else if sprintOk.contains(false) then fail(AppError(ErrorCode.TaskSprintNotFoundForAdd))
        else if parentOk.contains(false) then fail(AppError(ErrorCode.TaskParentNotFound))
        else
          (r.statusId.orElse(defaultStatus)) match {
            case None => fail(AppError(ErrorCode.TaskNoStatus))
            case Some(status) =>
              for {
                seq <-
                  sql"update project set task_seq = task_seq + 1 where id = $projectId returning task_seq, key".query[(
                    Long,
                    String
                  )].unique
                last <- sql"""select max(rank collate "C") from task where project_id = $projectId""".query[Option[
                  String
                ]].unique
                cat <- sql"select category from status where id = $status".query[String].unique
                _ <-
                  sql"""insert into task (id, project_id, number, key, parent_id, title, description, description_text, status_id, sprint_id,
                             assignee_id, reporter_id, start_date, due_date, rank, resolved_at, priority, type)
                           values ($id, $projectId, ${seq._1}, ${seq._2 + "-" +
                      seq._1}, ${r.parentId}, ${r.title.trim}, ${r.description},
                             $descText, $status, ${r.sprintId}, ${r.assigneeId}, $reporter, $start, $due,
                             ${FractionalIndex.after(last)}, ${Option.when(cat == StatusCategory.Done)(Instant.now())},
                             ${r.priority.filter(Priority.all.contains).getOrElse(Priority.Medium)},
                             ${r.taskType.filter(TaskType.all.contains).getOrElse(TaskType.Task)})""".update.run
                _ <- r.labelIds.distinct.traverse_(l =>
                  sql"insert into task_label (task_id, label_id) select $id, id from label where id = $l and project_id = $projectId".update.run
                )
                _ <- (reporter.toList ++ r.assigneeId.toList).distinct.traverse_(u =>
                  sql"insert into task_watcher (task_id, user_id) values ($id, $u) on conflict do nothing".update.run
                )
                fe <- setFieldsC(
                  projectId,
                  id,
                  r.fields.toList.flatMap((k, v) => uuidOpt(k).map(_ -> Option(v).filterNot(_.isNull)))
                )
                res <- fe match {
                  case Left(e) => fail[TaskCore](e)
                  case Right(_) => coreC(id).map(_.toRight(AppError(ErrorCode.TaskNotFound)))
                }
              } yield res
          }
    } yield res
  }

  /** カスタムフィールドの値を検証して保存する（値が None なら削除）。 */
  private def setFieldsC(projectId: UUID,
                         taskId: UUID,
                         values: List[(UUID, Option[Json])]): ConnectionIO[Either[AppError, Unit]] =
    if values.isEmpty then ().asRight[AppError].pure[ConnectionIO]
    else
      sql"select id, kind, options, name from custom_field where project_id = $projectId".query[(
        UUID,
        String,
        Json,
        String
      )].to[List].flatMap { defs =>
        val byId = defs.map(d => d._1 -> d).toMap
        val checked = values.traverse {
          (fid, v) =>
            byId.get(fid) match {
              case None => Left(AppError(ErrorCode.TaskFieldNotInProject, s"フィールド $fid はこのプロジェクトのものではありません"))
              case Some((_, kind, opts, name)) =>
                val options = opts.as[List[String]].getOrElse(Nil)
                v match {
                  case None => Right(fid -> None)
                  case Some(j) =>
                    val ok = kind match {
                      case FieldKind.Text | FieldKind.Textarea => j.isString
                      case FieldKind.Checkbox => j.as[List[String]].toOption.exists(_.forall(options.contains))
                      case _ => j.asString.exists(s => s.isEmpty || options.contains(s))
                    }
                    if ok then
                      Right(fid -> Some(j).filterNot(x => x.asString.contains("") || x.asArray.exists(_.isEmpty)))
                    else Left(AppError(ErrorCode.TaskFieldValueInvalid, s"フィールド「$name」の値が正しくありません"))
                }
            }
        }
        checked match {
          case Left(e) => fail(e)
          case Right(vs) =>
            vs.traverse_ {
              case (fid, None) =>
                sql"delete from task_field_value where task_id = $taskId and field_id = $fid".update.run
              case (fid, Some(j)) =>
                sql"""insert into task_field_value (task_id, field_id, value) values ($taskId, $fid, $j)
                                         on conflict (task_id, field_id) do update set value = excluded.value""".update.run
            }.map(_.asRight)
        }
      }

  // ---------------------------------------------------------------- 更新

  private def j[A: io.circe.Encoder](a: A): Json = a.asJson

  def update(id: UUID, ch: TaskChanges): IO[Either[AppError, UpdateResult]] = tx(updateC(id, ch))

  def updateC(id: UUID, ch: TaskChanges): ConnectionIO[Either[AppError, UpdateResult]] =
    (fr"select" ++ coreCols ++ fr"from task t where t.id = $id and t.deleted_at is null for update").query[
      TaskCore
    ].option.flatMap {
      case None => fail(AppError(ErrorCode.TaskNotFound))
      case Some(b) if ch.expectedVersion.exists(_ != b.version) =>
        fail(AppError(ErrorCode.TaskUpdateConflict))
      case Some(b) =>
        val pid = b.projectId
        def exists(q: Fragment) = q.query[Boolean].unique
        for {
          stOk <- ch.statusId.traverse(s =>
            exists(fr"select exists(select 1 from status where id = $s and project_id = $pid)")
          )
          spOk <- ch.sprintId.flatten.traverse(s =>
            exists(fr"select exists(select 1 from sprint where id = $s and project_id = $pid and state <> 'closed')")
          )
          paOk <- ch.parentId.flatten.traverse(p =>
            exists(
              fr"select exists(select 1 from task where id = $p and project_id = $pid and deleted_at is null)"
            ).flatMap { ok =>
              if !ok || p == id then false.pure[ConnectionIO] else descendantIdsC(id).map(ds => !ds.contains(p))
            }
          )
          lbOk <- ch.labelIds.traverse(ls =>
            sql"select count(*) from label where project_id = $pid and id = any(${ls.distinct.toArray})".query[
              Int
            ].unique.map(_ == ls.distinct.size)
          )
          res <-
            if stOk.contains(false) then fail(AppError(ErrorCode.TaskStatusNotInProject))
            else if spOk.contains(false) then fail(AppError(ErrorCode.TaskSprintNotFoundForMove))
            else if paOk.contains(false) then fail(AppError(ErrorCode.TaskParentInvalid))
            else if lbOk.contains(false) then fail(AppError(ErrorCode.TaskLabelNotInProject))
            else {
              val start = ch.startDate.getOrElse(b.startDate)
              val due = ch.dueDate.getOrElse(b.dueDate)
              if start.zip(due).exists((s, d) => s.isAfter(d))
              then fail(AppError(ErrorCode.TaskStartAfterDue))
              else applyC(b, ch)
            }
        } yield res
    }

  private def applyC(b: TaskCore, ch: TaskChanges): ConnectionIO[Either[AppError, UpdateResult]] = {
    val id = b.id
    for {
      newCat <- ch.statusId.filter(_ != b.statusId).traverse(s =>
        sql"select category from status where id = $s".query[String].unique
      )
      oldLabels <- sql"select label_id from task_label where task_id = $id".query[UUID].to[List]
      oldFields <-
        sql"select field_id, value from task_field_value where task_id = $id".query[(UUID, Json)].to[List].map(_.toMap)
      sets = List(
        ch.title.map(_.trim).filter(t => t.nonEmpty && t != b.title).map(v => fr"title = $v"),
        ch.description.map {
          case Some((d, t)) => fr"description = $d, description_text = $t"
          case None => fr"description = null, description_text = ''"
        },
        ch.statusId.filter(_ != b.statusId).map(v => fr"status_id = $v"),
        newCat.map(c =>
          if c == StatusCategory.Done then fr"resolved_at = coalesce(resolved_at, now())" else fr"resolved_at = null"
        ),
        ch.sprintId.filter(_ != b.sprintId).map(v => fr"sprint_id = $v"),
        ch.assigneeId.filter(_ != b.assigneeId).map(v => fr"assignee_id = $v"),
        ch.reporterId.filter(_ != b.reporterId).map(v => fr"reporter_id = $v"),
        ch.parentId.filter(_ != b.parentId).map(v => fr"parent_id = $v"),
        ch.startDate.filter(_ != b.startDate).map(v => fr"start_date = $v"),
        ch.dueDate.filter(_ != b.dueDate).map(v => fr"due_date = $v"),
        ch.rank.filter(_ != b.rank).map(v => fr"rank = $v"),
        ch.priority.filter(_ != b.priority).map(v => fr"priority = $v"),
        ch.taskType.filter(_ != b.taskType).map(v => fr"type = $v")
      ).flatten
      labelsChanged = ch.labelIds.exists(_.toSet != oldLabels.toSet)
      _ <-
      (sql"delete from task_label where task_id = $id".update.run *>
        ch.labelIds.getOrElse(Nil).distinct.traverse_(l =>
          sql"insert into task_label (task_id, label_id) values ($id, $l)".update.run
        ))
        .whenA(labelsChanged)
      changedFields = ch.fields.filter((k, v) => oldFields.get(k) != v)
      fe <- setFieldsC(b.projectId, id, changedFields.toList)
      res <- fe match {
        case Left(e) => fail[UpdateResult](e)
        case Right(_) =>
          val touched = sets.nonEmpty || labelsChanged || changedFields.nonEmpty
          for {
            _ <-
            (fr"update task" ++ Sql.set(sets ++ List(fr"version = version + 1", fr"updated_at = now()")) ++
              fr"where id = $id")
              .update.run.whenA(touched)
            a <- coreC(id).map(_.get)
          } yield {
            def c(name: String, from: Json, to: Json) = Option.when(from != to)(FieldChange(name, from, to))
            val changes = List(
              c("title", j(b.title), j(a.title)),
              Option.when(ch.description.isDefined && b.description != a.description)(FieldChange(
                "description",
                Json.Null,
                Json.Null
              )),
              c("statusId", j(b.statusId), j(a.statusId)),
              c("sprintId", j(b.sprintId), j(a.sprintId)),
              c("assigneeId", j(b.assigneeId), j(a.assigneeId)),
              c("reporterId", j(b.reporterId), j(a.reporterId)),
              c("parentId", j(b.parentId), j(a.parentId)),
              c("startDate", j(b.startDate.map(_.toString)), j(a.startDate.map(_.toString))),
              c("dueDate", j(b.dueDate.map(_.toString)), j(a.dueDate.map(_.toString))),
              c("priority", j(b.priority), j(a.priority)),
              c("taskType", j(b.taskType), j(a.taskType)),
              Option.when(labelsChanged)(FieldChange("labelIds", j(oldLabels), j(ch.labelIds.get.distinct)))
            ).flatten ++ changedFields.toList.map((k, v) =>
              FieldChange(s"fields.$k", oldFields.get(k).getOrElse(Json.Null), v.getOrElse(Json.Null))
            )
            Right(UpdateResult(b, a, changes))
          }
      }
    } yield res
  }

  /** カンバン・バックログでの移動（ステータス・スプリント・並び順）。 */
  def move(id: UUID, r: MoveTaskReq): IO[Either[AppError, UpdateResult]] = tx(
    coreC(id).flatMap {
      case None => fail(AppError(ErrorCode.TaskNotFound))
      case Some(b) =>
        val pid = b.projectId
        def rankOf(x: UUID) = sql"select rank from task where id = $x and project_id = $pid".query[String].option
        val rankC: ConnectionIO[Option[String]] = (r.afterId, r.beforeId) match {
          case (Some(a), Some(bf)) => (rankOf(a), rankOf(bf)).mapN((x, y) => Some(FractionalIndex.between(x, y)))
          case (Some(a), None) =>
            rankOf(a).flatMap(lo =>
              sql"""select min(rank collate "C") from task where project_id = $pid and id <> $id and rank collate "C" > ${lo.getOrElse(
                  ""
                )}"""
                .query[Option[String]].unique.map(hi => Some(FractionalIndex.between(lo, hi)))
            )
          case (None, Some(bf)) =>
            rankOf(bf).flatMap(hi =>
              sql"""select max(rank collate "C") from task where project_id = $pid and id <> $id and rank collate "C" < ${hi.getOrElse(
                  "~"
                )}"""
                .query[Option[String]].unique.map(lo => Some(FractionalIndex.between(lo, hi)))
            )
          case (None, None) => none[String].pure[ConnectionIO]
        }
        rankC.flatMap { rank =>
          updateC(
            id,
            TaskChanges(
              statusId = r.statusId,
              sprintId = if r.toBacklog.contains(true) then Some(None) else r.sprintId.map(Some(_)),
              rank = rank
            )
          )
        }
    }
  )

  /** 論理削除する（子孫も一緒に）。削除したタスクの ID を返す。 */
  def delete(id: UUID): IO[List[UUID]] = tx(
    descendantIdsC(id).flatMap { ds =>
      val all = id :: ds
      sql"update task set deleted_at = now(), updated_at = now() where id = any(${all.toArray}) and deleted_at is null".update.run.as(
        all
      )
    }
  )

  // ---------------------------------------------------------------- ウォッチャー

  def watch(taskId: UUID, userId: UUID): IO[Unit] = tx(
    sql"insert into task_watcher (task_id, user_id) values ($taskId, $userId) on conflict do nothing".update.run.void
  )

  def unwatch(taskId: UUID, userId: UUID): IO[Unit] =
    tx(sql"delete from task_watcher where task_id = $taskId and user_id = $userId".update.run.void)

  def watchers(taskId: UUID): IO[List[UUID]] =
    tx(sql"select user_id from task_watcher where task_id = $taskId".query[UUID].to[List])

  // ---------------------------------------------------------------- リンク

  /** 自分から張ったリンクと、相手から張られたリンク（種類を逆にする）。 */
  def linksC(taskId: UUID): ConnectionIO[List[TaskLinkDto]] =
    sql"""select l.id, l.kind, l.task_id = $taskId, t.id, t.key, t.title, s.name, s.category, l.url, l.title, l.created_at
          from task_link l
          left join task t on t.id = (case when l.task_id = $taskId then l.target_task_id else l.task_id end) and t.deleted_at is null
          left join status s on s.id = t.status_id
          where (l.task_id = $taskId or l.target_task_id = $taskId) and (l.target_task_id is null or t.id is not null)
          order by l.created_at"""
      .query[(UUID, String, Boolean, Option[TaskRef], Option[String], String, Instant)]
      .map((id, kind, mine, ref, url, title, at) =>
        TaskLinkDto(id, if mine then kind else LinkKind.inverse(kind), ref, url, title, at)
      )
      .to[List]

  def addLink(taskId: UUID,
              kind: String,
              target: Option[UUID],
              url: Option[String],
              title: String,
              by: UUID): IO[UUID] = tx {
    val id = UUID.randomUUID()
    sql"""insert into task_link (id, task_id, kind, target_task_id, url, title, created_by)
          values ($id, $taskId, $kind, $target, $url, $title, $by)""".update.run.as(id)
  }

  /** リンクを消す（どちらの側からでも）。 */
  def deleteLink(taskId: UUID, linkId: UUID): IO[Boolean] = tx(
    sql"delete from task_link where id = $linkId and (task_id = $taskId or target_task_id = $taskId)".update.run.map(_ >
      0)
  )

  // ---------------------------------------------------------------- 依存関係（ガント）

  /** 依存関係を追加する（循環するなら拒否）。 */
  def addDependency(pred: UUID, succ: UUID, kind: String): IO[Either[AppError, Unit]] = tx(
    if pred == succ then fail(AppError(ErrorCode.TaskDependsOnSelf))
    else
      sql"""with recursive chain(id, depth) as (
              select successor_id, 0 from task_dependency where predecessor_id = $succ
              union all select d.successor_id, c.depth + 1 from task_dependency d join chain c on d.predecessor_id = c.id where c.depth < 200)
            select exists(select 1 from chain where id = $pred)""".query[Boolean].unique.flatMap { cyc =>
        if cyc then fail(AppError(ErrorCode.TaskDependencyCycle))
        else
          sql"""insert into task_dependency (predecessor_id, successor_id, kind) values ($pred, $succ, $kind)
                on conflict (predecessor_id, successor_id) do update set kind = excluded.kind""".update.run.as(Right(
            ()
          ))
      }
  )

  def deleteDependency(pred: UUID, succ: UUID): IO[Boolean] =
    tx(sql"delete from task_dependency where predecessor_id = $pred and successor_id = $succ".update.run.map(_ > 0))

  // ---------------------------------------------------------------- 作業時間

  private val worklogSelect =
    fr"""select w.id, w.task_id, t.key, t.title, t.project_id, u.id, u.name, u.avatar_url, w.work_date::text, w.minutes, w.note, w.created_at
         from worklog w join task t on t.id = w.task_id join app_user u on u.id = w.user_id"""

  def worklogs(taskId: UUID): IO[List[WorklogDto]] = tx((worklogSelect ++
    fr"where w.task_id = $taskId order by w.work_date desc, w.created_at desc").query[WorklogDto].to[List])

  def worklog(id: UUID): IO[Option[WorklogDto]] = tx((worklogSelect ++ fr"where w.id = $id").query[WorklogDto].option)

  def addWorklog(taskId: UUID, userId: UUID, date: LocalDate, minutes: Int, note: String): IO[WorklogDto] = tx {
    val id = UUID.randomUUID()
    sql"insert into worklog (id, task_id, user_id, work_date, minutes, note) values ($id, $taskId, $userId, $date, $minutes, $note)".update.run *>
      (worklogSelect ++ fr"where w.id = $id").query[WorklogDto].unique
  }

  def updateWorklog(id: UUID, date: LocalDate, minutes: Int, note: String): IO[Unit] =
    tx(sql"update worklog set work_date = $date, minutes = $minutes, note = $note where id = $id".update.run.void)

  def deleteWorklog(id: UUID): IO[Unit] = tx(sql"delete from worklog where id = $id".update.run.void)

  /** 作業時間の一覧（日次の表）。projectIds は見られるプロジェクトに絞って渡すこと。 */
  def worklogReport(from: LocalDate,
                    to: LocalDate,
                    projectIds: List[UUID],
                    userId: Option[UUID]): IO[List[WorklogDto]] = tx(
    (worklogSelect ++
      fr"where w.work_date between $from and $to and t.project_id = any(${projectIds.toArray}) and t.deleted_at is null" ++
      userId.fold(
        Fragment.empty
      )(u => fr"and w.user_id = $u") ++ fr"order by w.work_date, u.name, t.key").query[WorklogDto].to[List]
  )

  // ---------------------------------------------------------------- コメント

  private val commentSelect =
    fr"""select c.id, c.task_id, u.id, u.name, u.avatar_url, c.body, c.via, c.created_at, c.updated_at, c.internal
         from comment c left join app_user u on u.id = c.author_id"""

  private type CommentRow = (UUID, UUID, Option[UserBrief], Json, String, Instant, Instant, Boolean)

  /** コメントごとのリアクション（絵文字の並びは最初に付けられた順）。 */
  private def reactionsC(ids: List[UUID]): ConnectionIO[Map[UUID, List[ReactionDto]]] =
    if ids.isEmpty then Map.empty[UUID, List[ReactionDto]].pure[ConnectionIO]
    else
      sql"""select r.comment_id, r.emoji, u.id, u.name, u.avatar_url from comment_reaction r join app_user u on u.id = r.user_id
            where r.comment_id = any(${ids.toArray}) order by r.created_at""".query[
        (UUID, String, UserBrief)
      ].to[List].map(_.groupBy(_._1).map {
        (cid, rs) => cid -> rs.map(_._2).distinct.map(e => ReactionDto(e, rs.filter(_._2 == e).map(_._3)))
      })

  private def commentDtos(rows: List[CommentRow]): ConnectionIO[List[CommentDto]] =
    reactionsC(rows.map(_._1)).map(rs =>
      rows.map(r =>
        CommentDto(r._1, r._2, r._3, r._4, RichText.toHtml(r._4), r._5, r._6, r._7, rs.getOrElse(r._1, Nil), r._8)
      )
    )

  /** タスクのコメント。withInternal が false なら内部コメント（エージェントだけに見えるもの）を除く（ヘルプデスクの"ユーザ"）。 */
  def comments(taskId: UUID, withInternal: Boolean = true): IO[List[CommentDto]] =
    tx((commentSelect ++ fr"where c.task_id = $taskId and c.deleted_at is null" ++
      (if withInternal then Fragment.empty else fr"and not c.internal") ++ fr"order by c.created_at").query[
      CommentRow
    ].to[List].flatMap(commentDtos))

  def comment(id: UUID): IO[Option[CommentDto]] = tx((commentSelect ++
    fr"where c.id = $id and c.deleted_at is null").query[CommentRow].to[List].flatMap(commentDtos).map(_.headOption))

  def addComment(taskId: UUID,
                 author: Option[UUID],
                 body: Json,
                 text: String,
                 via: String,
                 internal: Boolean = false): IO[CommentDto] = tx {
    val id = UUID.randomUUID()
    sql"""insert into comment (id, task_id, author_id, body, body_text, via, internal)
          values ($id, $taskId, $author, $body, $text, $via, $internal)""".update.run *>
      sql"update task set updated_at = now() where id = $taskId".update.run *>
      (commentSelect ++ fr"where c.id = $id").query[CommentRow].to[List].flatMap(commentDtos).map(_.head)
  }

  /** リアクションを付ける・外す。 */
  def react(commentId: UUID, userId: UUID, emoji: String, on: Boolean): IO[Unit] = tx(if on then
    sql"insert into comment_reaction (comment_id, user_id, emoji) values ($commentId, $userId, $emoji) on conflict do nothing".update.run.void
  else
    sql"delete from comment_reaction where comment_id = $commentId and user_id = $userId and emoji = $emoji".update.run.void)

  def updateComment(id: UUID, body: Json, text: String): IO[Unit] =
    tx(sql"update comment set body = $body, body_text = $text, updated_at = now() where id = $id".update.run.void)

  def deleteComment(id: UUID): IO[Unit] = tx(sql"update comment set deleted_at = now() where id = $id".update.run.void)

  def commentAuthor(id: UUID): IO[Option[(UUID, Option[UUID])]] =
    tx(sql"select task_id, author_id from comment where id = $id and deleted_at is null".query[(
      UUID,
      Option[UUID]
    )].option)

  // ---------------------------------------------------------------- 添付

  def addAttachment(taskId: UUID, key: String, name: String, size: Long, ct: String, sha: String, by: UUID): IO[UUID] =
    tx {
      val id = UUID.randomUUID()
      sql"""insert into attachment (id, task_id, s3_key, file_name, size, content_type, sha256, created_by)
          values ($id, $taskId, $key, $name, $size, $ct, $sha, $by)""".update.run.as(id)
    }

  def attachment(id: UUID): IO[Option[(UUID, String, String, String, Long)]] =
    tx(sql"select task_id, s3_key, file_name, content_type, size from attachment where id = $id".query[(
      UUID,
      String,
      String,
      String,
      Long
    )].option)

  def deleteAttachment(id: UUID): IO[Option[String]] =
    tx(sql"delete from attachment where id = $id returning s3_key".query[String].option)

  // ---------------------------------------------------------------- 外部のサービスからの参照（skiwi）

  /** タスクIDから (プロジェクト, キー, タイトル, ステータス名, 区分, 色)。 */
  def lookup(keys: List[String]): IO[List[(UUID, String, String, String, String, String)]] =
    if keys.isEmpty then IO.pure(Nil)
    else
      tx(sql"""select t.project_id, t.key, t.title, s.name, s.category, s.color from task t join status s on s.id = t.status_id
                  where t.key = any(${keys.map(_.trim.toUpperCase).distinct.toArray}) and t.deleted_at is null"""
        .query[(UUID, String, String, String, String, String)].to[List])

  // ---------------------------------------------------------------- 即時の反映（SSE）

  /** after より新しい変更履歴の (id, プロジェクト)。 */
  def eventsAfter(after: Long): IO[List[(Long, UUID)]] =
    tx(sql"select id, project_id from task_event where id > $after order by id limit 1000".query[(Long, UUID)].to[List])

  def latestEvent: IO[Long] = tx(sql"select coalesce(max(id), 0) from task_event".query[Long].unique)

  /** since より後に通知を受けたユーザーと、その中で最も新しい時刻。 */
  def notifiedSince(since: Instant): IO[List[(UUID, Instant)]] =
    tx(sql"select user_id, max(at) from notification where at > $since group by user_id".query[(
      UUID,
      Instant
    )].to[List])

  // ---------------------------------------------------------------- コメントの下書き

  def draft(userId: UUID, taskId: UUID): IO[Option[DraftDto]] = tx(
    sql"select body, updated_at from comment_draft where user_id = $userId and task_id = $taskId".query[DraftDto].option
  )

  def saveDraft(userId: UUID, taskId: UUID, body: Json): IO[Unit] = tx(
    sql"""insert into comment_draft (user_id, task_id, body) values ($userId, $taskId, $body)
          on conflict (user_id, task_id) do update set body = excluded.body, updated_at = now()""".update.run.void
  )

  def deleteDraft(userId: UUID, taskId: UUID): IO[Unit] =
    tx(sql"delete from comment_draft where user_id = $userId and task_id = $taskId".update.run.void)

  // ---------------------------------------------------------------- コミット

  def commitsC(taskId: UUID): ConnectionIO[List[CommitDto]] =
    sql"""select coalesce(c.repo_name, r.name), r.provider, c.sha, c.message, c.url, c.author, c.branch, c.committed_at
          from task_commit c join scm_repo r on r.id = c.repo_id where c.task_id = $taskId order by c.committed_at desc""".query[
      CommitDto
    ].to[List]

  /** コミットをタスクにリンクする。新しくリンクしたら true。 */
  def linkCommit(taskId: UUID,
                 repoId: UUID,
                 repoName: String,
                 sha: String,
                 message: String,
                 url: String,
                 author: String,
                 branch: Option[String],
                 at: Instant): IO[Boolean] = tx(
    sql"""insert into task_commit (task_id, repo_id, repo_name, sha, message, url, author, branch, committed_at)
          values ($taskId, $repoId, $repoName, $sha, $message, $url, $author, $branch, $at) on conflict do nothing""".update.run.map(
      _ > 0
    )
  )

  /** キーの一覧から、そのプロジェクトのタスクを探す。 */
  def idsByKeys(projectId: UUID, keys: List[String]): IO[Map[String, UUID]] =
    if keys.isEmpty then IO.pure(Map.empty)
    else
      tx(
        sql"select key, id from task where project_id = $projectId and key = any(${keys.toArray}) and deleted_at is null"
          .query[(String, UUID)].to[List]
      ).map(_.toMap)

  // ---------------------------------------------------------------- 変更履歴

  def eventC(projectId: UUID,
             taskId: Option[UUID],
             actor: Option[UUID],
             via: String,
             kind: String,
             detail: Json): ConnectionIO[Long] =
    sql"""insert into task_event (project_id, task_id, actor_id, via, kind, detail) values ($projectId, $taskId, $actor, $via, $kind, $detail)
          returning id""".query[Long].unique

  def event(projectId: UUID,
            taskId: Option[UUID],
            actor: Option[UUID],
            via: String,
            kind: String,
            detail: Json): IO[Long] =
    tx(eventC(projectId, taskId, actor, via, kind, detail))

  private val eventSelect =
    fr"""select e.id, e.project_id, e.task_id, t.key, u.id, u.name, u.avatar_url, e.via, e.kind, e.detail, e.at
         from task_event e left join task t on t.id = e.task_id left join app_user u on u.id = e.actor_id"""

  /** since より新しい変更（画面の自動同期）。 */
  def changes(projectId: UUID, since: Long, limit: Int): IO[ChangesDto] = tx(
    for {
      evs <- (eventSelect ++ fr"where e.project_id = $projectId and e.id > $since order by e.id limit $limit").query[
        EventDto
      ].to[List]
      latest <- sql"select coalesce(max(id), 0) from task_event where project_id = $projectId".query[Long].unique
    } yield ChangesDto(evs, latest)
  )

  /** タスクの変更履歴（新しい順）。 */
  def history(taskId: UUID, limit: Int): IO[List[EventDto]] =
    tx((eventSelect ++ fr"where e.task_id = $taskId order by e.id desc limit $limit").query[EventDto].to[List])

  /** プロジェクトの最近の活動。 */
  def activity(projectId: UUID, before: Option[Long], limit: Int): IO[List[EventDto]] =
    tx((eventSelect ++ fr"where e.project_id = $projectId" ++ before.fold(Fragment.empty)(b => fr"and e.id < $b") ++
      fr"order by e.id desc limit $limit").query[EventDto].to[List])

  // ---------------------------------------------------------------- 期限の通知

  /** 期日が今日・明日で未完了、または期限切れになったタスク（担当者あり）。 */
  def dueSoon(today: LocalDate): IO[List[(UUID, UUID, String, String, LocalDate, UUID)]] = tx(
    sql"""select t.id, t.project_id, t.key, t.title, t.due_date, t.assignee_id from task t join status s on s.id = t.status_id
          where t.deleted_at is null and s.category <> 'done' and t.assignee_id is not null
            and t.due_date between ${today.minusDays(1)} and ${today.plusDays(1)}"""
      .query[(UUID, UUID, String, String, LocalDate, UUID)].to[List]
  )
}
