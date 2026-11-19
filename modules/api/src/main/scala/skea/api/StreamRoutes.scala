package skea.api

import sbirds.shared.AppError

import cats.effect.*
import cats.syntax.all.*
import fs2.Stream
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import skea.core.*

import java.util.UUID
import scala.concurrent.duration.*

/** 画面に知らせる出来事（プロジェクトの変更、ユーザーへの通知）。 */
enum FeedItem {
  case Changed(
    projectId: UUID,
    eventId: Long)
  case Notified(
    userId: UUID)
}

final class StreamRoutes(feed: EventFeed, val sessions: SessionAuth, val access: Access2) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

    /** 画面の変更の通知（projectId を付けるとそのプロジェクトの変更も）。届いたら画面が変更を取りに行く。 */
    case req @ GET -> Root / "api" / "stream" => getStream(req)
  }

  private def sse(kind: String, data: Json): ServerSentEvent =
    ServerSentEvent(data = Some(data.noSpaces), eventType = Some(kind))

  private def getStream(req: Request[IO]): IO[Response[IO]] =
    authed(req) { s =>
      val pid = param(req, "projectId").flatMap(uuidOpt)
      pid.fold(IO.pure(Right(()): Either[AppError, Unit]))(p =>
        access.project(s, p, Role.View).map(_.void)
      ).flatMap {
        case Left(f) => fail(f)
        case _ =>
          val events = feed.subscribe.collect {
            case FeedItem.Changed(p, id) if pid.contains(p) => sse("change", Json.obj("latest" -> id.asJson))
            case FeedItem.Notified(u) if u == s.user.id => sse("notify", Json.obj())
          }
          // 接続が切られないよう 20 秒ごとに空の行を送る
          val ping = Stream.awakeEvery[IO](20.seconds).as(ServerSentEvent(comment = Some("ping")))
          val body = Stream.emit(ServerSentEvent(retry = Some(5.seconds), comment = Some("hello"))) ++
            events.merge(ping)
          // 接続中の数（skorimako のメトリクス）
          Ok(body.through(s =>
            Stream.bracket(sbirds.observe.Metrics.upDown("skea.sse.connections", 1))(_ =>
              sbirds.observe.Metrics.upDown("skea.sse.connections", -1)
            ) >> s
          ))
            .map(_.putHeaders("Cache-Control" -> "no-cache", "X-Accel-Buffering" -> "no"))
      }
    }
}
