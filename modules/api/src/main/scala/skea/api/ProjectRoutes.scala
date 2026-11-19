package skea.api

import sbirds.shared.AppError

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skea.core.*
import skea.db.*
import skea.shared.*

import java.time.LocalDate
import java.util.UUID

/** プロジェクトと、その設定・スプリント・カンバン・バックログ・ガントチャート・変更の同期。 */
final class ProjectRoutes(projects: ProjectStore,
                          tasks: TaskStore,
                          users: UserStore,
                          activity: ActivityStore,
                          svc: TaskService,
                          hub: EventHub,
                          val sessions: SessionAuth,
                          val access: Access2,
                          cfg: ApiConfig) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // ---------------------------------------------------------------- プロジェクト
    case req @ GET -> Root / "api" / "projects" => getProjects(req)
    case req @ POST -> Root / "api" / "projects" => postProjects(req)
    case req @ PUT -> Root / "api" / "projects" / "order" => putProjectsOrder(req)
    case req @ GET -> Root / "api" / "projects" / "by-key" / key => getProjectsByKey(req, key)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) => getProject(req, pid)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "meta" => getProjectMeta(req, pid)
    case req @ PATCH -> Root / "api" / "projects" / UuidVar(pid) => patchProject(req, pid)
    case req @ DELETE -> Root / "api" / "projects" / UuidVar(pid) => deleteProject(req, pid)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "members" => getProjectMembers(req, pid)
    case req @ PUT -> Root / "api" / "projects" / UuidVar(pid) / "members" => putProjectMembers(req, pid)
    case req @ DELETE -> Root / "api" / "projects" / UuidVar(pid) / "members" / UuidVar(uid) =>
      deleteProjectMember(req, pid, uid)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "groups" => getProjectGroups(req, pid)
    case req @ PUT -> Root / "api" / "projects" / UuidVar(pid) / "groups" => putProjectGroups(req, pid)
    case req @ DELETE -> Root / "api" / "projects" / UuidVar(pid) / "groups" / UuidVar(gid) =>
      deleteProjectGroup(req, pid, gid)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "mention-groups" => getProjectMentionGroups(req, pid)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "audience" => getProjectAudience(req, pid)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "statuses" => getProjectStatuses(req, pid)
    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "statuses" => postProjectStatuses(req, pid)
    case req @ PUT -> Root / "api" / "projects" / UuidVar(pid) / "statuses" / "order" =>
      putProjectStatusesOrder(req, pid)
    case req @ PUT -> Root / "api" / "projects" / UuidVar(pid) / "statuses" / UuidVar(id) =>
      putProjectStatuse(req, pid, id)
    case req @ DELETE -> Root / "api" / "projects" / UuidVar(pid) / "statuses" / UuidVar(id) =>
      deleteProjectStatuse(req, pid, id)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "labels" => getProjectLabels(req, pid)
    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "labels" => postProjectLabels(req, pid)
    case req @ PUT -> Root / "api" / "projects" / UuidVar(pid) / "labels" / UuidVar(id) => putProjectLabel(req, pid, id)
    case req @ DELETE -> Root / "api" / "projects" / UuidVar(pid) / "labels" / UuidVar(id) =>
      deleteProjectLabel(req, pid, id)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "fields" => getProjectFields(req, pid)
    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "fields" => postProjectFields(req, pid)
    case req @ PUT -> Root / "api" / "projects" / UuidVar(pid) / "fields" / "order" => putProjectFieldsOrder(req, pid)
    case req @ PUT -> Root / "api" / "projects" / UuidVar(pid) / "fields" / UuidVar(id) => putProjectField(req, pid, id)
    case req @ DELETE -> Root / "api" / "projects" / UuidVar(pid) / "fields" / UuidVar(id) =>
      deleteProjectField(req, pid, id)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "sprints" => getProjectSprints(req, pid)
    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "sprints" => postProjectSprints(req, pid)
    case req @ PUT -> Root / "api" / "projects" / UuidVar(pid) / "sprints" / "order" => putProjectSprintsOrder(req, pid)
    case req @ PUT -> Root / "api" / "projects" / UuidVar(pid) / "sprints" / UuidVar(id) =>
      putProjectSprint(req, pid, id)
    case req @ DELETE -> Root / "api" / "projects" / UuidVar(pid) / "sprints" / UuidVar(id) =>
      deleteProjectSprint(req, pid, id)
    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "sprints" / UuidVar(id) / "start" =>
      postProjectSprintStart(req, pid, id)
    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "sprints" / UuidVar(id) / "complete" =>
      postProjectSprintComplete(req, pid, id)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "board" => getProjectBoard(req, pid)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "backlog" => getProjectBacklog(req, pid)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "gantt" => getProjectGantt(req, pid)
    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "milestones" => postProjectMilestones(req, pid)
    case req @ PUT -> Root / "api" / "projects" / UuidVar(pid) / "milestones" / UuidVar(id) =>
      putProjectMilestone(req, pid, id)
    case req @ DELETE -> Root / "api" / "projects" / UuidVar(pid) / "milestones" / UuidVar(id) =>
      deleteProjectMilestone(req, pid, id)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "changes" => getProjectChanges(req, pid)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "activity" => getProjectActivity(req, pid)
    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "tasks" => postProjectTasks(req, pid)
    case req @ GET -> Root / "api" / "worklogs" => getWorklogs(req)
  }

  private def audit(s: AuthSession, action: String, target: UUID, detail: Json = Json.obj()) =
    activity.audit(Some(s.user.id), s.viaString, action, Some(target.toString), detail)

  private def validColor(c: Option[String]): Boolean = c.forall(_.matches("^#[0-9a-fA-F]{6}$"))

  private def statusReqOk(r: StatusReq): Option[String] =
    if r.name.trim.isEmpty then Some("名前を入力してください")
    else if !StatusCategory.all.contains(r.category) then Some("カテゴリは todo / in_progress / waiting / done です")
    else if !validColor(r.color) then Some("色は #RRGGBB で指定してください")
    else None

  private def fieldReqOk(r: CustomFieldReq): Option[String] =
    if r.name.trim.isEmpty then Some("名前を入力してください")
    else if !FieldKind.all.contains(r.kind) then Some("種類は text / textarea / checkbox / radio / select です")
    else if FieldKind.hasOptions(r.kind) && r.options.map(_.trim).filter(_.nonEmpty).isEmpty then
      Some("選択肢を1つ以上入力してください")
    else None

  private def milestoneOk(r: MilestoneReq): Either[String, LocalDate] =
    if r.name.trim.isEmpty then Left("名前を入力してください")
    else if !validColor(r.color) then Left("色は #RRGGBB で指定してください")
    else scala.util.Try(LocalDate.parse(r.date)).toEither.left.map(_ => "日付は yyyy-MM-dd で指定してください")

  private def sprintDates(r: SprintReq): Either[AppError, (Option[LocalDate], Option[LocalDate])] =
    (Dates.parse(r.startDate, "開始日"), Dates.parse(r.endDate, "終了日")).tupled.flatMap {
      (s, e) =>
        if s.zip(e).exists((a, b) => a.isAfter(b))
        then Left(AppError(ErrorCode.PeriodStartAfterEnd))
        else Right((s, e))
    }

  /** ヘルプデスクのプロジェクトでは、アクティブスプリントは自動で管理する（作成・開始・完了・削除・並べ替えはできない）。 */
  private def sprintsManual(pid: UUID)(f: => IO[Response[IO]]): IO[Response[IO]] =
    projects.helpdeskDoneDays(pid).flatMap {
      case Some(_) => err(ErrorCode.HelpdeskSprintManaged)
      case None => f
    }

  /** "ユーザ"（requester）の権限は、ヘルプデスクのプロジェクトでだけ付けられる。 */
  private def requesterOk(pid: UUID, role: Role): IO[Boolean] =
    if role.isAgent then IO.pure(true) else projects.dto(pid, role).map(_.exists(_.helpdesk))

  private def getProjects(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s => projects.list(s, param(req, "archived").contains("true")).flatMap(Ok(_)))

  private def postProjects(req: Request[IO]): IO[Response[IO]] =
    admin(req)(s =>
      body[CreateProjectReq](req) { r =>
        val key = r.key.trim.toUpperCase
        if !ProjectKey.valid(key) then err(ErrorCode.ProjectKeyInvalid)
        else if r.name.trim.isEmpty then err(ErrorCode.NameRequired)
        else
          projects.create(key, r.name.trim, r.description.getOrElse(""), s.user.id).flatMap {
            case Left(f) => fail(f)
            case Right(id) => audit(s, "project.create", id, Json.obj("key" -> key.asJson)) *>
                projects.dto(id, Role.Admin).flatMap(p => Created(p))
          }
      }
    )

  private def putProjectsOrder(req: Request[IO]): IO[Response[IO]] =
    admin(req)(_ => body[ReorderReq](req)(r => projects.reorder(r.ids) *> NoContent()))

  private def getProjectsByKey(req: Request[IO], key: String): IO[Response[IO]] =
    authed(req)(s =>
      projects.findByKey(key).flatMap {
        case None => err(ErrorCode.ProjectNotFound)
        case Some(p) => respond(access.project(s, p.id, Role.View))(r => projects.dto(p.id, r).flatMap(d => Ok(d)))
      }
    )

  private def getProject(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((_, r) => projects.dto(pid, r).flatMap(d => Ok(d)))

  private def getProjectMeta(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((_, r) => projects.meta(pid, r).flatMap(m => Ok(m)))

  private def patchProject(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((s, _) =>
      body[UpdateProjectReq](req) { r =>
        if r.archived.isDefined && !s.canAdmin then err(ErrorCode.ProjectArchiveAdminOnly)
        else projects.update(pid, r) *> audit(s, "project.update", pid, r.asJson) *> NoContent()
      }
    )

  private def deleteProject(req: Request[IO], pid: UUID): IO[Response[IO]] =
    admin(req)(s =>
      projects.find(pid).flatMap {
        case None => err(ErrorCode.ProjectNotFound)
        case Some(p) => projects.delete(pid) *> audit(s, "project.delete", pid, Json.obj("key" -> p.key.asJson)) *>
            NoContent()
      }
    )

  private def getProjectMembers(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((_, _) => projects.members(pid).flatMap(Ok(_)))

  private def putProjectMembers(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((s, _) =>
      body[SetMemberReq](req)(r =>
        Role.parse(r.role) match {
          case None => err(ErrorCode.MemberRoleInvalid)
          case Some(role) => requesterOk(pid, role).flatMap {
              case false => err(ErrorCode.MemberRequesterNeedsHelpdesk)
              case true =>
                users.find(r.userId).flatMap {
                  case None => err(ErrorCode.UserNotFound)
                  case Some(_) => projects.setMember(pid, r.userId, role) *>
                      audit(
                        s,
                        "project.member.set",
                        pid,
                        Json.obj("userId" -> r.userId.asJson, "role" -> r.role.asJson)
                      ) *> NoContent()
                }
            }
        }
      )
    )

  private def deleteProjectMember(req: Request[IO], pid: UUID, uid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((s, _) =>
      projects.removeMember(pid, uid) *> audit(s, "project.member.remove", pid, Json.obj("userId" -> uid.asJson)) *>
        NoContent()
    )

  private def getProjectGroups(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((_, _) => projects.groupRoles(pid).flatMap(Ok(_)))

  private def putProjectGroups(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((s, _) =>
      body[SetGroupRoleReq](req)(r =>
        (Role.parse(r.role), r.groupName.trim) match {
          case (None, _) => err(ErrorCode.MemberRoleInvalid)
          case (_, "") => err(ErrorCode.GroupNameRequired)
          case (Some(role), g) => requesterOk(pid, role).flatMap {
              case false => err(ErrorCode.MemberRequesterNeedsHelpdesk)
              case true =>
                users.groupByName(g).flatMap(gid => projects.setGroupRole(pid, gid, role)) *>
                  audit(s, "project.group.set", pid, Json.obj("group" -> g.asJson, "role" -> r.role.asJson)) *>
                  NoContent()
            }
        }
      )
    )

  private def deleteProjectGroup(req: Request[IO], pid: UUID, gid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((s, _) =>
      projects.removeGroupRole(pid, gid) *> audit(s, "project.group.remove", pid) *> NoContent()
    )

  private def getProjectMentionGroups(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((_, _) => projects.mentionGroups(pid, cfg.adminGroups).flatMap(Ok(_)))

  private def getProjectAudience(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((_, _) => projects.audience(pid, cfg.adminGroups).flatMap(Ok(_)))

  private def getProjectStatuses(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((_, _) => projects.statuses(pid).flatMap(Ok(_)))

  private def postProjectStatuses(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((_, _) =>
      body[StatusReq](req)(r =>
        statusReqOk(r) match {
          case Some(e) => err(ErrorCode.InvalidInput, e)
          case None => projects.addStatus(pid, r.copy(name = r.name.trim)).flatMap(_.fold(fail, Created(_)))
        }
      )
    )

  private def putProjectStatusesOrder(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((_, _) =>
      body[ReorderReq](req)(r => projects.reorderStatuses(pid, r.ids) *> NoContent())
    )

  private def putProjectStatuse(req: Request[IO], pid: UUID, id: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((_, _) =>
      body[StatusReq](req)(r =>
        statusReqOk(r) match {
          case Some(e) => err(ErrorCode.InvalidInput, e)
          case None => projects.updateStatus(
              pid,
              id,
              r.copy(name = r.name.trim)
            ).flatMap(_.fold(fail, _ => NoContent()))
        }
      )
    )

  private def deleteProjectStatuse(req: Request[IO], pid: UUID, id: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((_, _) =>
      param(req, "moveTo").flatMap(uuidOpt) match {
        case None => err(ErrorCode.StatusMoveToParamRequired)
        case Some(to) => projects.deleteStatus(pid, id, to).flatMap(_.fold(fail, _ => NoContent()))
      }
    )

  private def getProjectLabels(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((_, _) => projects.labels(pid).flatMap(Ok(_)))

  private def postProjectLabels(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Edit)((_, _) =>
      body[LabelReq](req)(r =>
        if r.name.trim.isEmpty then err(ErrorCode.NameRequired)
        else if !validColor(r.color) then err(ErrorCode.ColorInvalid)
        else projects.ensureLabel(pid, r.name, r.color).flatMap(Created(_))
      )
    )

  private def putProjectLabel(req: Request[IO], pid: UUID, id: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((_, _) =>
      body[LabelReq](req)(r =>
        if r.name.trim.isEmpty || !validColor(r.color) then err(ErrorCode.LabelNameOrColorInvalid)
        else projects.updateLabel(pid, id, r).flatMap(_.fold(fail, _ => NoContent()))
      )
    )

  private def deleteProjectLabel(req: Request[IO], pid: UUID, id: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((_, _) => projects.deleteLabel(pid, id) *> NoContent())

  private def getProjectFields(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((_, _) => projects.fields(pid).flatMap(Ok(_)))

  private def postProjectFields(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((_, _) =>
      body[CustomFieldReq](req)(r =>
        fieldReqOk(r) match {
          case Some(e) => err(ErrorCode.InvalidInput, e)
          case None => projects.addField(
              pid,
              r.copy(options = r.options.map(_.trim).filter(_.nonEmpty).distinct)
            ).flatMap(_.fold(fail, Created(_)))
        }
      )
    )

  private def putProjectFieldsOrder(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((_, _) =>
      body[ReorderReq](req)(r => projects.reorderFields(pid, r.ids) *> NoContent())
    )

  private def putProjectField(req: Request[IO], pid: UUID, id: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((_, _) =>
      body[CustomFieldReq](req)(r =>
        fieldReqOk(r) match {
          case Some(e) => err(ErrorCode.InvalidInput, e)
          case None => projects.updateField(
              pid,
              id,
              r.copy(options = r.options.map(_.trim).filter(_.nonEmpty).distinct)
            )
              .flatMap(_.fold(fail, _ => NoContent()))
        }
      )
    )

  private def deleteProjectField(req: Request[IO], pid: UUID, id: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((_, _) => projects.deleteField(pid, id) *> NoContent())

  private def getProjectSprints(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((_, _) =>
      projects.sprints(pid, param(req, "closed").contains("true")).flatMap(Ok(_))
    )

  private def postProjectSprints(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Edit)((s, _) =>
      sprintsManual(pid)(body[SprintReq](req)(r =>
        sprintDates(r) match {
          case Left(f) => fail(f)
          case Right(_) if r.name.trim.isEmpty => err(ErrorCode.NameRequired)
          case Right((st, en)) =>
            projects.createSprint(pid, r, st, en).flatTap(sp =>
              hub.emit(
                Op(s),
                pid,
                None,
                EventKind.SprintCreated,
                Json.obj("sprintId" -> sp.id.asJson, "name" -> sp.name.asJson)
              )
            ).flatMap(Created(_))
        }
      ))
    )

  private def putProjectSprintsOrder(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Edit)((_, _) =>
      sprintsManual(pid)(body[ReorderReq](req)(r => projects.reorderSprints(pid, r.ids) *> NoContent()))
    )

  private def putProjectSprint(req: Request[IO], pid: UUID, id: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Edit)((s, _) =>
      body[SprintReq](req)(r =>
        sprintDates(r) match {
          case Left(f) => fail(f)
          case Right(_) if r.name.trim.isEmpty => err(ErrorCode.NameRequired)
          case Right((st, en)) =>
            projects.sprint(id).flatMap {
              case Some(sp) if sp.projectId == pid =>
                projects.updateSprint(id, r, st, en) *>
                  hub.emit(Op(s), pid, None, "sprint.updated", Json.obj("sprintId" -> id.asJson)) *> NoContent()
              case _ => err(ErrorCode.SprintNotFound)
            }
        }
      )
    )

  private def deleteProjectSprint(req: Request[IO], pid: UUID, id: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Edit)((s, _) =>
      sprintsManual(pid)(projects.deleteSprint(pid, id).flatMap(_.fold(
        fail,
        _ =>
          hub.emit(Op(s), pid, None, "sprint.deleted", Json.obj("sprintId" -> id.asJson)) *> NoContent()
      )))
    )

  private def postProjectSprintStart(req: Request[IO], pid: UUID, id: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Edit)((s, _) =>
      sprintsManual(pid)(projects.startSprint(pid, id, Op(s).today).flatMap {
        case Left(f) => fail(f)
        case Right(sp) => hub.emit(
            Op(s),
            pid,
            None,
            EventKind.SprintStarted,
            Json.obj("sprintId" -> id.asJson, "name" -> sp.name.asJson)
          ) *> Ok(sp)
      })
    )

  private def postProjectSprintComplete(req: Request[IO], pid: UUID, id: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Edit)((s, _) =>
      sprintsManual(pid)(body[CompleteSprintReq](req)(r =>
        projects.completeSprint(pid, id, r.moveTo).flatMap {
          case Left(f) => fail(f)
          case Right(n) =>
            hub.emit(
              Op(s),
              pid,
              None,
              EventKind.SprintCompleted,
              Json.obj("sprintId" -> id.asJson, "moved" -> n.asJson, "moveTo" -> r.moveTo.asJson)
            ) *>
              Ok(Json.obj("moved" -> n.asJson))
        }
      ))
    )

  private def getProjectBoard(req: Request[IO], pid: UUID): IO[Response[IO]] = {
    // ヘルプデスクでは、完了して設定の日数を過ぎたタスクを出さない
    withProject(req, pid, Role.View)((_, _) =>
      (projects.sprints(pid, includeClosed = false), projects.helpdeskDoneDays(pid)).tupled.flatMap {
        (ss, hide) =>
          {
            val active =
              param(req, "sprintId").flatMap(uuidOpt).flatMap(id => ss.find(_.id == id)).orElse(ss.find(_.state ==
                "active"))
            active.fold(IO.pure(List.empty[TaskDto]))(sp => tasks.board(pid, sp.id, hide)).flatMap(ts =>
              Ok(BoardDto(active, ts))
            )
          }
      }
    )
  }

  private def getProjectBacklog(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((_, _) =>
      (projects.sprints(pid, includeClosed = false), tasks.backlog(pid)).tupled.flatMap((ss, ts) =>
        Ok(BacklogDto(ss, ts))
      )
    )

  private def getProjectGantt(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((_, _) => tasks.gantt(pid, param(req, "done").contains("true")).flatMap(Ok(_)))

  private def postProjectMilestones(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Edit)((s, _) =>
      body[MilestoneReq](req)(r =>
        milestoneOk(r) match {
          case Left(m) => err(ErrorCode.InvalidInput, m)
          case Right(d) => projects.createMilestone(pid, r.name.trim, d, r.color.getOrElse("#dc2626")).flatTap(m =>
              hub.emit(
                Op(s),
                pid,
                None,
                "milestone.created",
                Json.obj("name" -> m.name.asJson, "date" -> m.date.asJson)
              )
            ).flatMap(Created(_))
        }
      )
    )

  private def putProjectMilestone(req: Request[IO], pid: UUID, id: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Edit)((s, _) =>
      body[MilestoneReq](req)(r =>
        milestoneOk(r) match {
          case Left(m) => err(ErrorCode.InvalidInput, m)
          case Right(d) => projects.updateMilestone(pid, id, r.name.trim, d, r.color.getOrElse("#dc2626")).flatMap {
              case true => hub.emit(
                  Op(s),
                  pid,
                  None,
                  "milestone.updated",
                  Json.obj("name" -> r.name.trim.asJson, "date" -> d.toString.asJson)
                ) *> NoContent()
              case false => err(ErrorCode.MilestoneNotFound)
            }
        }
      )
    )

  private def deleteProjectMilestone(req: Request[IO], pid: UUID, id: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Edit)((s, _) =>
      projects.deleteMilestone(pid, id).flatMap {
        case true => hub.emit(Op(s), pid, None, "milestone.deleted", Json.obj()) *> NoContent()
        case false => err(ErrorCode.MilestoneNotFound)
      }
    )

  private def getProjectChanges(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((_, _) =>
      tasks.changes(
        pid,
        param(req, "since").flatMap(_.toLongOption).getOrElse(0L),
        intParam(req, "limit", 100, 500)
      ).flatMap(Ok(_))
    )

  private def getProjectActivity(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((_, _) =>
      tasks.activity(
        pid,
        param(req, "before").flatMap(_.toLongOption),
        intParam(req, "limit", 50, 200)
      ).flatMap(Ok(_))
    )

  private def postProjectTasks(req: Request[IO], pid: UUID): IO[Response[IO]] =
    authed(req)(s => body[CreateTaskReq](req)(r => respond(svc.create(Op(s), pid, r))(Created(_))))

  private def getWorklogs(req: Request[IO]): IO[Response[IO]] =
    authed(req) { s =>
      val today = Op(s).today
      (Dates.parse(param(req, "from"), "from"), Dates.parse(param(req, "to"), "to")).tupled match {
        case Left(f) => fail(f)
        case Right((from, to)) =>
          val f = from.getOrElse(today.minusDays(today.getDayOfWeek.getValue - 1))
          val t = to.getOrElse(f.plusDays(6))
          if t.isBefore(f) || f.plusDays(370).isBefore(t) then err(ErrorCode.PeriodTooLong)
          else
            projects.visible(s).flatMap { vis =>
              val ps = param(req, "projectId").flatMap(uuidOpt).fold(vis.keys.toList)(p =>
                vis.keys.filter(_ == p).toList
              )
              tasks.worklogReport(f, t, ps, param(req, "userId").flatMap(uuidOpt)).flatMap(Ok(_))
            }
      }
    }
}
