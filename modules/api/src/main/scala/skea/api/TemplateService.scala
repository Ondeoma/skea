package skea.api

import sbirds.db.*
import sbirds.observe.Log

import cats.effect.*
import cats.syntax.all.*
import skea.core.*
import skea.db.*
import skea.shared.*

import java.time.{LocalDate, ZoneId}
import java.util.UUID
import scala.concurrent.duration.*

/** テンプレートからのタスクの作成と、繰り返しタスク（決まった日に作る）。 */
final class TemplateService(templates: TemplateStore,
                            projects: ProjectStore,
                            svc: TaskService,
                            sessions: SessionAuth,
                            lock: JobLock,
                            cfg: ApiConfig) {

  /** テンプレートからタスク（とサブタスク）を作る。date は {{date}} と期日の起点。 */
  def instantiate(op: Op, tpl: TemplateDto, projectId: UUID, date: LocalDate, activeSprint: Boolean): Res[TaskDto] = {
    val title = tpl.title.replace("{{date}}", date.toString).replace("{{month}}", date.toString.take(7))
    val sprint = if activeSprint then
      projects.sprints(projectId, includeClosed = false).map(_.find(_.state == "active").map(_.id))
    else IO.pure(None)
    sprint.flatMap { sp =>
      val req = CreateTaskReq(
        title = title,
        description = tpl.description,
        statusId = tpl.statusId,
        sprintId = sp,
        assigneeId = tpl.assigneeId,
        dueDate = tpl.dueInDays.map(d => date.plusDays(d.toLong).toString),
        labelIds = tpl.labelIds,
        fields = tpl.fields,
        priority = tpl.priority,
        taskType = tpl.taskType
      )
      svc.create(op, projectId, req).flatMap {
        case Left(f) => IO.pure(Left(f))
        case Right(t) =>
          tpl.subtasks.traverse_(st =>
            svc.create(
              op,
              projectId,
              CreateTaskReq(
                title = st,
                parentId = Some(t.id),
                sprintId = sp,
                assigneeId = tpl.assigneeId
              )
            ).void
          ).as(Right(t))
      }
    }
  }

  def nextRun(frequency: String, weekday: Option[Int], monthday: Option[Int], after: LocalDate): LocalDate =
    Recurrence.next(frequency, weekday, monthday, after)

  def validate(r: RecurringReq): Either[String, Unit] =
    if !Frequency.all.contains(r.frequency) then Left("頻度は daily / weekly / monthly です")
    else if r.frequency == Frequency.Weekly && !r.weekday.exists(d => d >= 1 && d <= 7) then Left("毎週の曜日を選んでください")
    else if r.frequency == Frequency.Monthly && !r.monthday.exists(d => d >= 1 && d <= 31) then Left("毎月の日を選んでください")
    else Right(())

  def today: LocalDate = LocalDate.now(ZoneId.of(cfg.timezone))

  /** 作る日が来た繰り返しタスクを作る（作成者として動く。止まっていた間の分はまとめて1つだけ作る）。 */
  def runDue: IO[Unit] = {
    val now = today
    templates.dueRecurring(now).flatMap(_.traverse_ { r =>
      val next = nextRun(r.frequency, r.weekday, r.monthday, now)
      (templates.template(r.templateId), r.createdBy.flatTraverse(u => sessions.sessionFor(u))).tupled.flatMap {
        case (Some((tpl, TemplateOwner.Project(_))), Some(s)) =>
          instantiate(Op(s), tpl, r.projectId, now, r.activeSprint).flatMap {
            case Right(t) => templates.markRun(r.id, r.nextRun, next, Some(t.key)).void
            case Left(f) => Log.report(ErrorCode.RecurringTaskFailed, s"${r.id}: ${f.message}") *>
                templates.markRun(r.id, r.nextRun, next, None).void
          }
        case _ => Log.report(ErrorCode.RecurringTaskSourceMissing, r.id.toString) *>
            templates.markRun(r.id, r.nextRun, next, None).void
      }
    })
  }

  /** 5分ごとに確認する（複数ノードでも1つだけが動く）。 */
  def loop: IO[Nothing] =
    Periodic.run(30.seconds, 5.minutes, ErrorCode.RecurringTasksFailed)(lock.exclusive("recurring", 4.minutes)(runDue))
}
