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

final case class WebhookRow(id: UUID, projectId: UUID, name: String, url: String, secret: String, events: List[String], enabled: Boolean)
final case class DeliveryRow(id: UUID, webhookId: UUID, url: String, secret: String, event: String, payload: Json, attempts: Int)

final case class RuleRow(id: UUID, projectId: UUID, name: String, enabled: Boolean, trigger: Json, actions: Json, runAs: Option[UUID],
    createdBy: Option[UUID], incomingToken: Option[String], lastRunAt: Option[Instant], createdAt: Instant)
final case class QueueRow(id: UUID, ruleId: UUID, taskId: Option[UUID], actorId: Option[UUID], event: Json, depth: Int, attempts: Int)

final case class RepoRow(id: UUID, projectId: UUID, provider: String, name: String, webUrl: String, apiUrl: Option[String], secret: String,
    tokenEnc: Option[String], branch: Option[String], lastPolledAt: Option[Instant], lastSha: Option[String], allRepos: Boolean)

/** Webhook（送信）・自動化・SCM の設定と送信待ち。 */
final class IntegrationStore(xa: Transactor[IO]) extends TxStore(xa):

  // ---------------------------------------------------------------- Webhook

  private val hookCols = fr"select id, project_id, name, url, secret, events, enabled from webhook"

  def webhooks(projectId: UUID): IO[List[WebhookRow]] = tx((hookCols ++ fr"where project_id = $projectId order by name").query[WebhookRow].to[List])

  def webhook(id: UUID): IO[Option[WebhookRow]] = tx((hookCols ++ fr"where id = $id").query[WebhookRow].option)

  def createWebhook(projectId: UUID, r: WebhookReq, secret: String): IO[UUID] = tx {
    val id = UUID.randomUUID()
    sql"""insert into webhook (id, project_id, name, url, secret, events, enabled)
          values ($id, $projectId, ${r.name}, ${r.url}, $secret, ${r.events.toArray}, ${r.enabled})""".update.run.as(id)
  }

  def updateWebhook(id: UUID, r: WebhookReq): IO[Unit] = tx(
    sql"update webhook set name = ${r.name}, url = ${r.url}, events = ${r.events.toArray}, enabled = ${r.enabled} where id = $id".update.run.void
  )

  def deleteWebhook(id: UUID): IO[Unit] = tx(sql"delete from webhook where id = $id".update.run.void)

  /** イベントを、購読している有効な Webhook の送信待ちに積む。 */
  def enqueueDeliveries(projectId: UUID, event: String, payload: Json): IO[Int] = tx(
    sql"""insert into webhook_delivery (id, webhook_id, event, payload)
          select gen_random_uuid(), id, $event, $payload from webhook
          where project_id = $projectId and enabled and (cardinality(events) = 0 or $event = any(events))""".update.run
  )

  def deliveries(webhookId: UUID, limit: Int): IO[List[DeliveryDto]] = tx(
    sql"""select id, event, attempts, status_code, last_error, done_at, failed, created_at, payload from webhook_delivery
          where webhook_id = $webhookId order by created_at desc limit $limit""".query[DeliveryDto].to[List]
  )

  def processDeliveries(batch: Int)(send: DeliveryRow => IO[(OutboxResult, Option[Int])]): IO[Int] =
    processEach(
      sql"""select d.id, d.webhook_id, w.url, w.secret, d.event, d.payload, d.attempts from webhook_delivery d join webhook w on w.id = d.webhook_id
            where d.done_at is null and not d.failed and d.not_before <= now() order by d.not_before limit $batch for update of d skip locked"""
        .query[DeliveryRow].to[List]
    )(d => send(d).handleError(e => (OutboxResult.Retry(e.getMessage), None))) {
      case (d, (OutboxResult.Sent, code)) =>
        sql"update webhook_delivery set done_at = now(), attempts = attempts + 1, status_code = $code where id = ${d.id}".update.run
      case (d, (OutboxResult.Skip(r), code)) =>
        sql"update webhook_delivery set failed = true, last_error = $r, status_code = $code where id = ${d.id}".update.run
      case (d, (OutboxResult.Retry(err), code)) =>
        sql"""update webhook_delivery set attempts = attempts + 1, last_error = $err, status_code = $code, failed = ${d.attempts + 1 >= 8},
              not_before = now() + make_interval(secs => ${Outbox.backoffSeconds(d.attempts)}) where id = ${d.id}""".update.run
    }

  def purgeDeliveries(days: Int): IO[Int] = tx(
    sql"delete from webhook_delivery where created_at < now() - make_interval(days => $days) and (done_at is not null or failed)".update.run
  )

  // ---------------------------------------------------------------- 自動化

  private val ruleCols =
    fr"select id, project_id, name, enabled, trigger, actions, run_as, created_by, incoming_token, last_run_at, created_at from automation_rule"

  def rules(projectId: UUID): IO[List[RuleRow]] = tx((ruleCols ++ fr"where project_id = $projectId order by name").query[RuleRow].to[List])

  def rule(id: UUID): IO[Option[RuleRow]] = tx((ruleCols ++ fr"where id = $id").query[RuleRow].option)

  def ruleByToken(token: String): IO[Option[RuleRow]] = tx((ruleCols ++ fr"where incoming_token = $token and enabled").query[RuleRow].option)

  /** トリガーの種類が type の有効なルール。 */
  def rulesFor(projectId: UUID, tpe: String): IO[List[RuleRow]] = tx(
    (ruleCols ++ fr"where project_id = $projectId and enabled and trigger->>'type' = $tpe").query[RuleRow].to[List]
  )

  def scheduledRules: IO[List[RuleRow]] = tx((ruleCols ++ fr"where enabled and trigger->>'type' = 'scheduled'").query[RuleRow].to[List])

  def createRule(projectId: UUID, r: RuleReq, by: UUID, token: Option[String]): IO[UUID] = tx {
    val id = UUID.randomUUID()
    sql"""insert into automation_rule (id, project_id, name, enabled, trigger, actions, run_as, created_by, incoming_token)
          values ($id, $projectId, ${r.name}, ${r.enabled}, ${r.trigger}, ${r.actions}, ${r.runAs}, $by, $token)""".update.run.as(id)
  }

  def updateRule(id: UUID, r: RuleReq, token: Option[String]): IO[Unit] = tx(
    sql"""update automation_rule set name = ${r.name}, enabled = ${r.enabled}, trigger = ${r.trigger}, actions = ${r.actions},
          run_as = ${r.runAs}, incoming_token = coalesce(incoming_token, $token), updated_at = now() where id = $id""".update.run.void
  )

  def deleteRule(id: UUID): IO[Unit] = tx(sql"delete from automation_rule where id = $id".update.run.void)

  def touchRule(id: UUID, at: Instant): IO[Unit] = tx(sql"update automation_rule set last_run_at = $at where id = $id".update.run.void)

  def enqueueRun(ruleId: UUID, taskId: Option[UUID], actor: Option[UUID], event: Json, depth: Int): IO[Unit] = tx(
    sql"""insert into automation_queue (id, rule_id, task_id, actor_id, event, depth)
          values (${UUID.randomUUID()}, $ruleId, $taskId, $actor, $event, $depth)""".update.run.void
  )

  /** 実行待ちを1件ずつ取り出して run で処理する（SKIP LOCKED）。 */
  def processQueue(batch: Int)(run: QueueRow => IO[Unit]): IO[Int] =
    processEach(
      sql"""select id, rule_id, task_id, actor_id, event, depth, attempts from automation_queue
            where done_at is null and not_before <= now() order by created_at limit $batch for update skip locked""".query[QueueRow].to[List]
    )(q => run(q).attempt) {
      case (q, Right(_)) => sql"update automation_queue set done_at = now(), attempts = attempts + 1 where id = ${q.id}".update.run
      case (q, Left(_))  =>
        sql"""update automation_queue set attempts = attempts + 1, done_at = case when attempts + 1 >= 3 then now() else null end,
              not_before = now() + interval '1 minute' where id = ${q.id}""".update.run
    }

  def purgeQueue(days: Int): IO[Int] = tx(
    sql"delete from automation_queue where done_at < now() - make_interval(days => $days)".update.run *>
      sql"delete from automation_run where started_at < now() - make_interval(days => ${days * 3})".update.run
  )

  def saveRun(ruleId: UUID, taskId: Option[UUID], status: String, log: Json, started: Instant): IO[Unit] = tx(
    sql"""insert into automation_run (id, rule_id, task_id, status, log, started_at, ended_at)
          values (${UUID.randomUUID()}, $ruleId, $taskId, $status, $log, $started, now())""".update.run.void
  )

  def runs(projectId: UUID, ruleId: Option[UUID], limit: Int): IO[List[RunDto]] = tx(
    (fr"""select r.id, r.rule_id, a.name, t.key, r.status, r.log, r.started_at, r.ended_at
          from automation_run r join automation_rule a on a.id = r.rule_id left join task t on t.id = r.task_id
          where a.project_id = $projectId""" ++ ruleId.fold(Fragment.empty)(id => fr"and r.rule_id = $id") ++
      fr"order by r.started_at desc limit $limit").query[RunDto].to[List]
  )

  // ---------------------------------------------------------------- SCM

  private val repoCols = fr"select id, project_id, provider, name, web_url, api_url, secret, token_enc, branch, last_polled_at, last_sha, all_repos from scm_repo"

  def repos(projectId: UUID): IO[List[RepoRow]] = tx((repoCols ++ fr"where project_id = $projectId order by name").query[RepoRow].to[List])

  def repo(id: UUID): IO[Option[RepoRow]] = tx((repoCols ++ fr"where id = $id").query[RepoRow].option)

  def pollableRepos: IO[List[RepoRow]] = tx((repoCols ++ fr"where token_enc is not null").query[RepoRow].to[List])

  def createRepo(projectId: UUID, r: RepoReq, secret: String, tokenEnc: Option[String]): IO[UUID] = tx {
    val id = UUID.randomUUID()
    sql"""insert into scm_repo (id, project_id, provider, name, web_url, api_url, secret, token_enc, branch, all_repos)
          values ($id, $projectId, ${r.provider}, ${r.name}, ${r.webUrl}, ${r.apiUrl}, $secret, $tokenEnc, ${r.branch}, ${r.allRepos.getOrElse(false)})""".update.run.as(id)
  }

  /** tokenEnc が None のときはトークンを変えない。clearToken なら消す。 */
  def updateRepo(id: UUID, r: RepoReq, tokenEnc: Option[String], clearToken: Boolean): IO[Unit] = tx(
    sql"""update scm_repo set provider = ${r.provider}, name = ${r.name}, web_url = ${r.webUrl}, api_url = ${r.apiUrl}, branch = ${r.branch}, all_repos = ${r.allRepos.getOrElse(false)},
          token_enc = case when $clearToken then null else coalesce($tokenEnc, token_enc) end where id = $id""".update.run.void
  )

  def deleteRepo(id: UUID): IO[Unit] = tx(sql"delete from scm_repo where id = $id".update.run.void)

  def markPolled(id: UUID, lastSha: Option[String]): IO[Unit] = tx(
    sql"update scm_repo set last_polled_at = now(), last_sha = coalesce($lastSha, last_sha) where id = $id".update.run.void
  )
