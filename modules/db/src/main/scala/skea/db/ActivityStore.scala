package skea.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.postgres.circe.jsonb.implicits.*
import io.circe.Json
import skea.shared.*

import java.time.Instant
import java.util.UUID

/** 送信待ちのメール。 */
final case class MailRow(id: UUID, userId: Option[UUID], toAddress: Option[String], projectId: Option[UUID], subject: String, body: String,
    attempts: Int)

/** 画面内の通知・メールの送信待ち・監査ログ。 */
final class ActivityStore(xa: Transactor[IO]) extends TxStore(xa):

  // ---------------------------------------------------------------- 通知

  def notify(userId: UUID, kind: String, projectId: Option[UUID], taskId: Option[UUID], commentId: Option[UUID], actor: Option[UUID],
      title: String, body: String): IO[UUID] = tx {
    val id = UUID.randomUUID()
    sql"""insert into notification (id, user_id, kind, project_id, task_id, comment_id, actor_id, title, body)
          values ($id, $userId, $kind, $projectId, $taskId, $commentId, $actor, $title, $body)""".update.run.as(id)
  }

  private val notifSelect =
    fr"""select n.id, n.kind, p.key, n.task_id, t.key, n.comment_id, u.id, u.name, u.avatar_url, n.title, n.body, n.at, n.read_at is not null
         from notification n left join project p on p.id = n.project_id left join task t on t.id = n.task_id
         left join app_user u on u.id = n.actor_id"""

  /** kinds が空ならすべて。projectIds は今見られるプロジェクト（権限を外されたプロジェクトの通知は出さない）。 */
  def list(userId: UUID, kinds: List[String], unreadOnly: Boolean, projectIds: List[UUID], before: Option[Instant], limit: Int): IO[NotificationListDto] =
    tx(
      for
        items <- (notifSelect ++ fr"where n.user_id = $userId and (n.project_id is null or n.project_id = any(${projectIds.toArray}))" ++
                   (if kinds.isEmpty then Fragment.empty else fr"and n.kind = any(${kinds.toArray})") ++
                   (if unreadOnly then fr"and n.read_at is null" else Fragment.empty) ++
                   before.fold(Fragment.empty)(b => fr"and n.at < $b") ++ fr"order by n.at desc limit $limit").query[NotificationDto].to[List]
        unread <- sql"select count(*) from notification where user_id = $userId and read_at is null".query[Int].unique
      yield NotificationListDto(items, unread)
    )

  def unread(userId: UUID): IO[UnreadDto] = tx(
    sql"""select count(*), count(*) filter (where kind = 'mention') from notification where user_id = $userId and read_at is null"""
      .query[(Int, Int)].unique.map(UnreadDto.apply)
  )

  def markRead(userId: UUID, ids: List[UUID]): IO[Unit] = tx(
    sql"update notification set read_at = now() where user_id = $userId and id = any(${ids.toArray}) and read_at is null".update.run.void
  )

  def markAllRead(userId: UUID, kinds: List[String]): IO[Unit] = tx(
    (fr"update notification set read_at = now() where user_id = $userId and read_at is null" ++
      (if kinds.isEmpty then Fragment.empty else fr"and kind = any(${kinds.toArray})")).update.run.void
  )

  /** タスクを開いたら、そのタスクの通知を既読にする。 */
  def markTaskRead(userId: UUID, taskId: UUID): IO[Unit] = tx(
    sql"update notification set read_at = now() where user_id = $userId and task_id = $taskId and read_at is null".update.run.void
  )

  /** 同じ日・同じタスクの期限の通知はしない（重複防止）。 */
  def dueNotified(userId: UUID, taskId: UUID, since: Instant): IO[Boolean] = tx(
    sql"select exists(select 1 from notification where user_id = $userId and task_id = $taskId and kind = 'due' and at >= $since)".query[Boolean].unique
  )

  def purgeOld(days: Int): IO[Int] = tx(
    sql"delete from notification where at < now() - make_interval(days => $days)".update.run
  )

  // ---------------------------------------------------------------- メール

  /** dedupe_key が同じ未送信のメールがあれば、本文を差し替えて送信を遅らせる（まとめて1通にする）。 */
  def enqueueMail(userId: Option[UUID], to: Option[String], projectId: Option[UUID], subject: String, body: String, dedupe: Option[String],
      delaySeconds: Int): IO[Unit] = tx {
    val id = UUID.randomUUID()
    dedupe match
      case Some(k) =>
        sql"""insert into mail_outbox (id, user_id, to_address, project_id, subject, body, dedupe_key, not_before)
              values ($id, $userId, $to, $projectId, $subject, $body, $k, now() + make_interval(secs => $delaySeconds))
              on conflict (dedupe_key) where sent_at is null and not skipped and dedupe_key is not null
              do update set subject = excluded.subject, body = excluded.body,
                not_before = least(excluded.not_before, mail_outbox.created_at + interval '30 minutes')""".update.run.void
      case None =>
        sql"""insert into mail_outbox (id, user_id, to_address, project_id, subject, body, not_before)
              values ($id, $userId, $to, $projectId, $subject, $body, now() + make_interval(secs => $delaySeconds))""".update.run.void
  }

  /** 送信する分を取り出し（SKIP LOCKED で複数ノードの重複を防ぐ）、send の結果を記録する。 */
  def processMail(batch: Int)(send: MailRow => IO[OutboxResult]): IO[Int] =
    processEach(
      sql"""select id, user_id, to_address, project_id, subject, body, attempts from mail_outbox
            where sent_at is null and not skipped and not_before <= now() order by not_before limit $batch for update skip locked""".query[MailRow].to[List]
    )(m => send(m).handleError(e => OutboxResult.Retry(e.getMessage))) {
      case (m, OutboxResult.Sent)         => sql"update mail_outbox set sent_at = now(), attempts = attempts + 1 where id = ${m.id}".update.run
      case (m, OutboxResult.Skip(reason)) => sql"update mail_outbox set skipped = true, last_error = $reason where id = ${m.id}".update.run
      case (m, OutboxResult.Retry(err))   =>
        sql"""update mail_outbox set attempts = attempts + 1, last_error = $err, skipped = ${m.attempts + 1 >= 10},
              not_before = now() + make_interval(secs => ${Outbox.backoffSeconds(m.attempts)}) where id = ${m.id}""".update.run
    }

  /** メールを送ってよいか（ユーザーが有効で、その種類のメールを止めていない）。宛先のアドレスを返す。 */
  def mailTarget(userId: UUID, kind: Option[String]): IO[Option[(String, String)]] = tx(
    sql"""select u.email, u.name from app_user u where u.id = $userId and not u.disabled and u.email is not null
          and not exists(select 1 from notify_pref p where p.user_id = u.id and p.kind = ${kind.getOrElse("")} and not p.email)"""
      .query[(String, String)].option
  )

  // ---------------------------------------------------------------- 監査ログ

  def audit(actor: Option[UUID], via: String, action: String, target: Option[String], detail: Json): IO[Unit] = tx(
    sql"insert into audit_log (actor_id, via, action, target, detail) values ($actor, $via, $action, $target, $detail)".update.run.void
  )

  def auditLog(before: Option[Long], action: Option[String], limit: Int): IO[List[AuditDto]] = tx(
    (fr"""select a.id, a.at, u.id, u.name, u.avatar_url, a.via, a.action, a.target, a.detail
          from audit_log a left join app_user u on u.id = a.actor_id where true""" ++
      before.fold(Fragment.empty)(b => fr"and a.id < $b") ++ action.fold(Fragment.empty)(x => fr"and a.action like ${x + "%"}") ++
      fr"order by a.id desc limit $limit").query[AuditDto].to[List]
  )
