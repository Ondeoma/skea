package skea.api

import cats.effect.*
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.Topic
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import skea.core.*
import skea.db.*

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

/** 画面に知らせる出来事（プロジェクトの変更、ユーザーへの通知）。 */
enum FeedItem:
  case Changed(projectId: UUID, eventId: Long)
  case Notified(userId: UUID)

/**
 * 変更の配信（SSE）。各ノードが1秒ごとに新しい変更履歴と通知を DB で確かめ、そのノードにつないでいる画面へ知らせる
 * （ノードどうしの連絡は不要。DB がどのノードの変更も持っているため）。
 */
final class EventFeed(tasks: TaskStore, topic: Topic[IO, FeedItem]):

  def loop: IO[Nothing] =
    (tasks.latestEvent, IO.realTimeInstant).tupled.flatMap { (start, now) =>
      def step(last: Long, since: Instant): IO[(Long, Instant)] =
        for
          evs <- tasks.eventsAfter(last)
          _   <- evs.groupMapReduce(_._2)(_._1)(math.max).toList.traverse_((p, id) => topic.publish1(FeedItem.Changed(p, id)))
          ns  <- tasks.notifiedSince(since)
          _   <- ns.traverse_((u, _) => topic.publish1(FeedItem.Notified(u)))
        yield (evs.lastOption.fold(last)(_._1), (since :: ns.map(_._2)).max)
      def go(last: Long, since: Instant): IO[Nothing] =
        IO.sleep(1.second) *> step(last, since).handleErrorWith(e => IO.println(s"[skea] event feed failed: $e").as((last, since)))
          .flatMap((l, s) => go(l, s))
      go(start, now)
    }

  def subscribe: Stream[IO, FeedItem] = topic.subscribe(256)

final class StreamRoutes(feed: EventFeed, val sessions: SessionAuth, val access: Access2) extends RouteSupport:

  private def sse(kind: String, data: Json): ServerSentEvent = ServerSentEvent(data = Some(data.noSpaces), eventType = Some(kind))

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    /** 画面の変更の通知（projectId を付けるとそのプロジェクトの変更も）。届いたら画面が変更を取りに行く。 */
    case req @ GET -> Root / "api" / "stream" =>
      authed(req) { s =>
        val pid = param(req, "projectId").flatMap(uuidOpt)
        pid.fold(IO.pure(Right(()): Either[Fail, Unit]))(p => access.project(s, p, Role.View).map(_.void)).flatMap {
          case Left(f) => fail(f)
          case _ =>
            val events = feed.subscribe.collect {
              case FeedItem.Changed(p, id) if pid.contains(p) => sse("change", Json.obj("latest" -> id.asJson))
              case FeedItem.Notified(u) if u == s.user.id    => sse("notify", Json.obj())
            }
            // 接続が切られないよう 20 秒ごとに空の行を送る
            val ping = Stream.awakeEvery[IO](20.seconds).as(ServerSentEvent(comment = Some("ping")))
            val body = Stream.emit(ServerSentEvent(retry = Some(5.seconds), comment = Some("hello"))) ++ events.merge(ping)
            Ok(body).map(_.putHeaders("Cache-Control" -> "no-cache", "X-Accel-Buffering" -> "no"))
        }
      }
  }
