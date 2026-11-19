package skea.db

import skea.core.ErrorCode

import sbirds.shared.AppError

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.postgres.circe.jsonb.implicits.*
import io.circe.Json
import io.circe.syntax.*
import skea.shared.*

import java.time.{Instant, LocalDate}
import java.util.UUID

/** テンプレートの持ち主。 */
enum TemplateOwner {
  case Project(
    id: UUID)
  case User(
    id: UUID)
}

final case class RecurringRow(
  id: UUID,
  projectId: UUID,
  templateId: UUID,
  frequency: String,
  weekday: Option[Int],
  monthday: Option[Int],
  activeSprint: Boolean,
  nextRun: LocalDate,
  enabled: Boolean,
  lastRunAt: Option[Instant],
  lastTaskKey: Option[String],
  createdBy: Option[UUID])

/** タスクのテンプレートと、繰り返しタスク。 */
final class TemplateStore(
  xa: Transactor[IO]) extends TxStore(xa) {

  // ---------------------------------------------------------------- テンプレート

  private val templateCols =
    fr"""select id, name, title, description, status_id, assignee_id, label_ids, fields, subtasks, due_in_days, priority, task_type, project_id,
           user_id from task_template"""

  private type TemplateRow = (
    UUID,
    String,
    String,
    Option[Json],
    Option[UUID],
    Option[UUID],
    List[UUID],
    Json,
    Json,
    Option[Int],
    Option[String],
    Option[String],
    Option[UUID],
    Option[UUID]
  )

  private def dto(
    r: TemplateRow
  ): (TemplateDto, TemplateOwner) =
    (
      TemplateDto(
        r._1,
        r._2,
        r._3,
        r._4,
        r._5,
        r._6,
        r._7,
        r._8.as[Map[String, Json]].getOrElse(Map.empty),
        r._9.as[List[String]].getOrElse(Nil),
        r._10,
        r._11,
        r._12,
        r._14.isDefined
      ),
      r._14.fold(TemplateOwner.Project(r._13.get))(TemplateOwner.User(_))
    )

  def templates(
    projectId: UUID
  ): IO[List[TemplateDto]] =
    tx(
      (templateCols ++ fr"where project_id = $projectId order by name").query[TemplateRow].to[List]
    ).map(_.map(dto(_)._1))

  /** 個人のテンプレート。 */
  def personalTemplates(
    userId: UUID
  ): IO[List[TemplateDto]] =
    tx((templateCols ++ fr"where user_id = $userId order by name").query[TemplateRow].to[List]).map(_.map(dto(_)._1))

  /** テンプレートと、その持ち主（プロジェクトか個人）。 */
  def template(
    id: UUID
  ): IO[Option[(TemplateDto, TemplateOwner)]] =
    tx((templateCols ++ fr"where id = $id").query[TemplateRow].option).map(_.map(dto))

  private def unique[A](
    io: ConnectionIO[A]
  ): ConnectionIO[Either[AppError, A]] =
    io.attemptSqlState.map(_.left.map {
      case s if s.value == "23505" => AppError(ErrorCode.TemplateNameTaken)
      case s => AppError(ErrorCode.SaveRejected, s"保存できませんでした（${s.value}）")
    })

  def createTemplate(
    owner: TemplateOwner,
    r0: TemplateReq,
    by: UUID
  ): IO[Either[AppError, UUID]] = tx {
    val id = UUID.randomUUID()
    val r = owner match { case TemplateOwner.User(_) => r0.personal; case _ => r0 }
    val (pid, uid) =
      owner match { case TemplateOwner.Project(p) => (Some(p), None); case TemplateOwner.User(u) => (None, Some(u)) }
    unique(sql"""insert into task_template (id, project_id, user_id, name, title, description, status_id, assignee_id, label_ids, fields, subtasks,
                   due_in_days, priority, task_type, created_by)
                 values ($id, $pid, $uid, ${r.name.trim}, ${r.title.trim}, ${r.description}, ${r.statusId}, ${r.assigneeId}, ${r.labelIds},
                   ${r.fields.asJson}, ${r.subtasks.map(
        _.trim
      ).filter(_.nonEmpty).asJson}, ${r.dueInDays}, ${r.priority}, ${r.taskType},
                   $by)""".update.run.as(id))
  }

  def updateTemplate(
    id: UUID,
    owner: TemplateOwner,
    r0: TemplateReq
  ): IO[Either[AppError, Unit]] = tx {
    val r = owner match { case TemplateOwner.User(_) => r0.personal; case _ => r0 }
    unique(sql"""update task_template set name = ${r.name.trim}, title = ${r.title.trim}, description = ${r.description}, status_id = ${r.statusId},
                   assignee_id = ${r.assigneeId}, label_ids = ${r.labelIds}, fields = ${r.fields.asJson},
                   subtasks = ${r.subtasks.map(
        _.trim
      ).filter(_.nonEmpty).asJson}, due_in_days = ${r.dueInDays}, priority = ${r.priority},
                   task_type = ${r.taskType} where id = $id""".update.run.void)
  }

  def deleteTemplate(
    id: UUID
  ): IO[Unit] = tx(sql"delete from task_template where id = $id".update.run.void)

  // ---------------------------------------------------------------- 繰り返しタスク

  private val recurringCols =
    fr"""select r.id, r.project_id, r.template_id, r.frequency, r.weekday, r.monthday, r.active_sprint, r.next_run, r.enabled, r.last_run_at,
           r.last_task_key, r.created_by from recurring_task r"""

  def recurring(
    projectId: UUID
  ): IO[List[RecurringDto]] = tx(
    sql"""select r.id, r.template_id, t.name, r.frequency, r.weekday, r.monthday, r.active_sprint, r.next_run::text, r.enabled, r.last_run_at,
            r.last_task_key
          from recurring_task r join task_template t on t.id = r.template_id where r.project_id = $projectId order by r.next_run"""
      .query[RecurringDto].to[List]
  )

  def recurringRow(
    id: UUID
  ): IO[Option[RecurringRow]] = tx((recurringCols ++ fr"where r.id = $id").query[RecurringRow].option)

  def createRecurring(
    projectId: UUID,
    r: RecurringReq,
    nextRun: LocalDate,
    by: UUID
  ): IO[UUID] = tx {
    val id = UUID.randomUUID()
    sql"""insert into recurring_task (id, project_id, template_id, frequency, weekday, monthday, active_sprint, next_run, enabled, created_by)
          values ($id, $projectId, ${r.templateId}, ${r.frequency}, ${r.weekday}, ${r.monthday}, ${r.activeSprint}, $nextRun, ${r.enabled}, $by)"""
      .update.run.as(id)
  }

  def updateRecurring(
    id: UUID,
    r: RecurringReq,
    nextRun: LocalDate
  ): IO[Unit] = tx(
    sql"""update recurring_task set template_id = ${r.templateId}, frequency = ${r.frequency}, weekday = ${r.weekday}, monthday = ${r.monthday},
            active_sprint = ${r.activeSprint}, enabled = ${r.enabled}, next_run = $nextRun where id = $id""".update.run.void
  )

  def deleteRecurring(
    id: UUID
  ): IO[Unit] = tx(sql"delete from recurring_task where id = $id".update.run.void)

  /** 作る日が来たもの。 */
  def dueRecurring(
    today: LocalDate
  ): IO[List[RecurringRow]] =
    tx((recurringCols ++ fr"where r.enabled and r.next_run <= $today order by r.next_run limit 100").query[
      RecurringRow
    ].to[List])

  /** 作ったことを記録し、次に作る日に進める（ほかのノードが先に進めていたら false）。 */
  def markRun(
    id: UUID,
    ran: LocalDate,
    next: LocalDate,
    taskKey: Option[String]
  ): IO[Boolean] = tx(
    sql"""update recurring_task set next_run = $next, last_run_at = now(), last_task_key = coalesce($taskKey, last_task_key)
          where id = $id and next_run = $ran""".update.run.map(_ > 0)
  )
}
