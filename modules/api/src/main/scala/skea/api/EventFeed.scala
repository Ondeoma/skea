package skea.api

import sbirds.observe.Log

import cats.effect.*
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.Topic
import skea.core.*
import skea.db.*

import java.time.Instant
import scala.concurrent.duration.*

/**
 * 変更の配信（SSE）。各ノードが1秒ごとに新しい変更履歴と通知を DB で確かめ、そのノードにつないでいる画面へ知らせる
 * （ノードどうしの連絡は不要。DB がどのノードの変更も持っているため）。
 */
final class EventFeed(tasks: TaskStore, topic: Topic[IO, FeedItem]) {

  def loop: IO[Nothing] =
    (tasks.latestEvent, IO.realTimeInstant).tupled.flatMap {
      (start, now) =>
        {
          def step(last: Long, since: Instant): IO[(Long, Instant)] =
            for {
              evs <- tasks.eventsAfter(last)
              _ <- evs.groupMapReduce(_._2)(_._1)(math.max).toList.traverse_((p, id) =>
                topic.publish1(FeedItem.Changed(p, id))
              )
              ns <- tasks.notifiedSince(since)
              _ <- ns.traverse_((u, _) => topic.publish1(FeedItem.Notified(u)))
            } yield (evs.lastOption.fold(last)(_._1), (since :: ns.map(_._2)).max)
          def go(last: Long, since: Instant): IO[Nothing] =
            IO.sleep(1.second) *> step(last, since).handleErrorWith(e =>
              Log.report(ErrorCode.EventFeedFailed, "", e).as((last, since))
            )
              .flatMap((l, s) => go(l, s))
          go(start, now)
        }
    }

  def subscribe: Stream[IO, FeedItem] = topic.subscribe(256)
}
