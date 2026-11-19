package skea.api

import cats.effect.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skea.core.*
import skea.db.*
import skea.shared.*

import java.time.Instant
import java.util.UUID

/** 検索・保存した検索条件・通知。 */
final class SearchRoutes(search: SearchStore,
                         projects: ProjectStore,
                         activity: ActivityStore,
                         val sessions: SessionAuth,
                         val access: Access2)
  extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "search" => getSearch(req)
    case req @ GET -> Root / "api" / "filters" => getFilters(req)
    case req @ POST -> Root / "api" / "filters" => postFilters(req)
    case req @ DELETE -> Root / "api" / "filters" / UuidVar(id) => deleteFilter(req, id)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "board-filters" => getProjectBoardFilters(req, pid)
    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "board-filters" => postProjectBoardFilters(req, pid)
    case req @ DELETE -> Root / "api" / "board-filters" / UuidVar(id) => deleteBoardFilter(req, id)
    case req @ GET -> Root / "api" / "notifications" => getNotifications(req)
    case req @ GET -> Root / "api" / "notifications" / "unread" => getNotificationsUnread(req)
    case req @ POST -> Root / "api" / "notifications" / "read" => postNotificationsRead(req)
    case req @ POST -> Root / "api" / "notifications" / "read-all" => postNotificationsReadAll(req)
  }

  private def getSearch(req: Request[IO]): IO[Response[IO]] =
    authed(req) { s =>
      SearchQuery.parse(param(req, "q").getOrElse("")) match {
        case Left(e) => Ok(SearchResultDto(Nil, 0, Some(e)))
        case Right(q) =>
          projects.visible(s).flatMap { vis =>
            val ps =
              param(req, "projectId").flatMap(uuidOpt).fold(vis.keys.toList)(p => vis.keys.filter(_ == p).toList)
            search.search(
              q,
              search.Ctx(s.user.id, ps, Op(s).today),
              intParam(req, "limit", 50, 500),
              param(req, "offset").flatMap(_.toIntOption).getOrElse(0)
            )
              .flatMap {
                case Left(e) => Ok(SearchResultDto(Nil, 0, Some(e)))
                case Right(r) => Ok(r)
              }
          }
      }
    }

  private def getFilters(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s => search.savedFilters(s.user.id).flatMap(Ok(_)))

  private def postFilters(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s =>
      body[SavedFilterReq](req)(r =>
        if r.name.trim.isEmpty || r.query.trim.isEmpty then err(ErrorCode.FilterNameAndQueryRequired)
        else search.saveFilter(s.user.id, r.name.trim, r.query.trim).flatMap(Created(_))
      )
    )

  private def deleteFilter(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req)(s => search.deleteFilter(s.user.id, id) *> NoContent())

  private def getProjectBoardFilters(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((s, _) => search.boardFilters(pid, s.user.id).flatMap(Ok(_)))

  private def postProjectBoardFilters(req: Request[IO], pid: UUID): IO[Response[IO]] = {
    // 自分用は閲覧権限で、プロジェクトで共有するものは編集権限で保存できる
    withProject(req, pid, Role.View)((s, role) =>
      body[BoardFilterReq](req)(r =>
        if r.name.trim.isEmpty || r.name.length > 50 then err(ErrorCode.FilterNameTooLong)
        else if r.cond.isEmpty then err(ErrorCode.FilterQueryMissing)
        else if r.cond.productIterator.exists(_.toString.length > 200) then err(ErrorCode.FilterQueryTooLong)
        else if r.shared && role < Role.Edit then err(ErrorCode.FilterShareNeedsEdit)
        else search.saveBoardFilter(pid, s.user.id, r).flatMap(_.fold(fail, Created(_)))
      )
    )
  }

  private def deleteBoardFilter(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req)(s =>
      search.boardFilter(id).flatMap {
        case Some((pid, Some(uid), _)) if uid == s.user.id =>
          respond(access.project(s, pid, Role.View))(_ => search.deleteBoardFilter(id) *> NoContent())
        case Some((pid, None, by)) =>
          // 共有のものは、作った人か、プロジェクトの管理者が消せる
          respond(access.project(s, pid, if by.contains(s.user.id) then Role.Edit else Role.Admin))(_ =>
            search.deleteBoardFilter(id) *> NoContent()
          )
        case _ => err(ErrorCode.FilterNotFound)
      }
    )

  private def getNotifications(req: Request[IO]): IO[Response[IO]] =
    authed(req) { s =>
      val kinds =
        param(req, "kinds").toList.flatMap(_.split(',').toList.map(_.trim)).filter(NotificationKind.all.contains)
      val before = param(req, "before").flatMap(b => scala.util.Try(Instant.parse(b)).toOption)
      // ヘルプデスクの"ユーザ"として参加しているプロジェクトの通知も出す
      projects.memberships(s).flatMap(vis =>
        activity.list(
          s.user.id,
          kinds,
          param(req, "unread").contains("true"),
          vis.keys.toList,
          before,
          intParam(req, "limit", 50, 200)
        ).flatMap(Ok(_))
      )
    }

  private def getNotificationsUnread(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s => activity.unread(s.user.id).flatMap(Ok(_)))

  private def postNotificationsRead(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s => body[MarkReadReq](req)(r => activity.markRead(s.user.id, r.ids) *> NoContent()))

  private def postNotificationsReadAll(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s =>
      activity.markAllRead(s.user.id, param(req, "kinds").toList.flatMap(_.split(',').toList)) *> NoContent()
    )
}
