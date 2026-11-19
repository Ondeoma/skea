package skea.api

import sbirds.shared.AppError

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skea.core.*
import skea.db.*
import skea.shared.*

import java.time.LocalDate
import java.util.UUID

final class TemplateRoutes(templates: TemplateStore,
                           svc: TemplateService,
                           imports: ImportService,
                           val sessions: SessionAuth,
                           val access: Access2)
  extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // ---------------------------------------------------------------- テンプレート
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "templates" => getProjectTemplates(req, pid)
    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "templates" => postProjectTemplates(req, pid)
    case req @ GET -> Root / "api" / "me" / "templates" => getMeTemplates(req)
    case req @ POST -> Root / "api" / "me" / "templates" => postMeTemplates(req)
    case req @ PUT -> Root / "api" / "templates" / UuidVar(id) => putTemplate(req, id)
    case req @ DELETE -> Root / "api" / "templates" / UuidVar(id) => deleteTemplate(req, id)
    case req @ POST -> Root / "api" / "templates" / UuidVar(id) / "tasks" => postTemplateTasks(req, id)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "recurring" => getProjectRecurring(req, pid)
    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "recurring" => postProjectRecurring(req, pid)
    case req @ PUT -> Root / "api" / "recurring" / UuidVar(id) => putRecurring(req, id)
    case req @ DELETE -> Root / "api" / "recurring" / UuidVar(id) => deleteRecurring(req, id)
    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "import" => postProjectImport(req, pid)
  }

  /** テンプレートを操作する（プロジェクトのものはプロジェクトの権限、個人のものは本人だけ）。 */
  private def withTemplate(req: Request[IO],
                           id: UUID,
                           need: Role)(f: (AuthSession, TemplateDto, TemplateOwner) => IO[Response[IO]]) =
    authed(req)(s =>
      templates.template(id).flatMap {
        case Some((t, o @ TemplateOwner.Project(pid))) => respond(access.project(s, pid, need))(_ => f(s, t, o))
        case Some((t, o @ TemplateOwner.User(uid))) if uid == s.user.id => f(s, t, o)
        case _ => err(ErrorCode.TemplateNotFound)
      }
    )

  private def templateOk(r: TemplateReq): Option[String] =
    if r.name.trim.isEmpty || r.title.trim.isEmpty then Some("名前とタイトルを入力してください")
    else if r.dueInDays.exists(d => d < 0 || d > 3650) then Some("期日は 0〜3650 日後で指定してください")
    else if r.name.length > 100 || r.title.length > 500 then Some("名前は 100 文字、タイトルは 500 文字までです")
    else if r.subtasks.size > 50 || r.subtasks.exists(_.length > 500) then Some("サブタスクは 50 件（1件 500 文字）までです")
    else if r.description.exists(d => !RichText.valid(d)) then Some("説明の形式が正しくありません")
    else if r.priority.exists(Priority.parse(_).isEmpty) then Some("優先度が正しくありません")
    else if r.taskType.exists(TaskType.parse(_).isEmpty) then Some("種類が正しくありません")
    else None

  private def withRecurring(req: Request[IO], id: UUID)(f: (AuthSession, RecurringRow) => IO[Response[IO]]) =
    authed(req)(s =>
      templates.recurringRow(id).flatMap {
        case None => err(ErrorCode.RecurringNotFound)
        case Some(r) => respond(access.project(s, r.projectId, Role.Edit))(_ => f(s, r))
      }
    )

  /** 最初に作る日（startDate があればその日以降、なければ今日より後）。 */
  private def firstRun(r: RecurringReq): Either[String, LocalDate] =
    r.startDate.filter(_.nonEmpty) match {
      case Some(d) => scala.util.Try(LocalDate.parse(d)).toEither.left.map(_ => "開始日は yyyy-MM-dd で指定してください")
          .map(d0 => svc.nextRun(r.frequency, r.weekday, r.monthday, d0.minusDays(1)))
      case None => Right(svc.nextRun(r.frequency, r.weekday, r.monthday, svc.today))
    }

  private def getProjectTemplates(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((_, _) => templates.templates(pid).flatMap(Ok(_)))

  private def postProjectTemplates(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Edit)((s, _) =>
      body[TemplateReq](req)(r =>
        templateOk(r) match {
          case Some(m) => err(ErrorCode.InvalidInput, m)
          case None => templates.createTemplate(TemplateOwner.Project(pid), r, s.user.id).flatMap(_.fold(
              fail,
              id =>
                templates.template(id).flatMap(t => Created(t.map(_._1)))
            ))
        }
      )
    )

  private def getMeTemplates(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s => templates.personalTemplates(s.user.id).flatMap(Ok(_)))

  private def postMeTemplates(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s =>
      body[TemplateReq](req)(r =>
        templateOk(r) match {
          case Some(m) => err(ErrorCode.InvalidInput, m)
          case None => templates.createTemplate(TemplateOwner.User(s.user.id), r, s.user.id).flatMap(_.fold(
              fail,
              id =>
                templates.template(id).flatMap(t => Created(t.map(_._1)))
            ))
        }
      )
    )

  private def putTemplate(req: Request[IO], id: UUID): IO[Response[IO]] =
    withTemplate(req, id, Role.Edit)((_, _, o) =>
      body[TemplateReq](req)(r =>
        templateOk(r) match {
          case Some(m) => err(ErrorCode.InvalidInput, m)
          case None => templates.updateTemplate(id, o, r).flatMap(_.fold(fail, _ => NoContent()))
        }
      )
    )

  private def deleteTemplate(req: Request[IO], id: UUID): IO[Response[IO]] =
    withTemplate(req, id, Role.Edit)((_, _, _) => templates.deleteTemplate(id) *> NoContent())

  private def postTemplateTasks(req: Request[IO], id: UUID): IO[Response[IO]] =
    withTemplate(req, id, Role.Edit)((s, t, o) => {
      val target = o match {
        case TemplateOwner.Project(pid) => IO.pure(Right(pid))
        case TemplateOwner.User(_) => param(req, "projectId").flatMap(uuidOpt) match {
            case Some(pid) => access.project(s, pid, Role.Edit).map(_.as(pid))
            case None => IO.pure(Left(AppError(ErrorCode.TemplateTargetProjectRequired)))
          }
      }
      respond(target)(pid =>
        respond(svc.instantiate(Op(s), t, pid, svc.today, param(req, "activeSprint").contains("true")))(Created(_))
      )
    })

  private def getProjectRecurring(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((_, _) => templates.recurring(pid).flatMap(Ok(_)))

  private def postProjectRecurring(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Edit)((s, _) =>
      body[RecurringReq](req)(r =>
        (svc.validate(r), firstRun(r)).tupled match {
          case Left(m) => err(ErrorCode.InvalidInput, m)
          case Right((_, first)) => templates.template(r.templateId).flatMap {
              case Some((_, TemplateOwner.Project(tp))) if tp == pid =>
                templates.createRecurring(pid, r, first, s.user.id) *> templates.recurring(pid).flatMap(Created(_))
              case _ => err(ErrorCode.TemplateNotFoundForCreate)
            }
        }
      )
    )

  private def putRecurring(req: Request[IO], id: UUID): IO[Response[IO]] =
    withRecurring(req, id)((_, old) =>
      body[RecurringReq](req)(r =>
        (svc.validate(r), firstRun(r)).tupled match {
          case Left(m) => err(ErrorCode.InvalidInput, m)
          case Right((_, first)) =>
            // 頻度・曜日・日を変えなければ、次に作る日はそのまま
            val same = old.frequency == r.frequency && old.weekday == r.weekday && old.monthday == r.monthday &&
              r.startDate.forall(_.isEmpty)
            // テンプレートは同じプロジェクトのものだけ（ほかのプロジェクトのテンプレートを使わせない）
            templates.template(r.templateId).flatMap {
              case Some((_, TemplateOwner.Project(tp))) if tp == old.projectId =>
                templates.updateRecurring(id, r, if same then old.nextRun else first) *> NoContent()
              case _ => err(ErrorCode.TemplateNotFoundForCreate)
            }
        }
      )
    )

  private def deleteRecurring(req: Request[IO], id: UUID): IO[Response[IO]] =
    withRecurring(req, id)((_, _) => templates.deleteRecurring(id) *> NoContent())

  private def postProjectImport(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Edit)((s, _) => body[ImportReq](req)(r => respond(imports.run(Op(s), pid, r))(Ok(_))))
}
