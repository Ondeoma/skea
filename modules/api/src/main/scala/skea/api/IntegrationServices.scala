package skea.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.{Authorization, `Content-Type`}
import skea.auth.{Crypto, SecretBox}
import skea.core.*
import skea.db.*
import skea.integrations.*
import skea.shared.*

import java.nio.charset.StandardCharsets
import java.util.UUID
import scala.concurrent.duration.*

/** 連携（Webhook・stui・SCM）の呼び出しの数（skorimako のメトリクス skea.integrations.calls）。 */
object Integrations:
  def called(integration: String, result: String): IO[Unit] =
    skea.observe.Metrics.count("skea.integrations.calls", "integration" -> integration, "result" -> result)

/** Webhook の送信（アウトボックスのワーカー）。本文に HMAC-SHA256 の署名を付ける。 */
final class WebhookService(integ: IntegrationStore, http: Client[IO], cfg: ApiConfig):

  def validUrl(u: String): Either[Fail, String] =
    val t = u.trim
    if t.startsWith("https://") || (cfg.webhookAllowHttp && t.startsWith("http://")) then
      Uri.fromString(t).left.map(_ => Fail.Invalid("URL が正しくありません")).map(_ => t)
    else Left(Fail.Invalid(if cfg.webhookAllowHttp then "URL は http(s):// で始めてください" else "URL は https:// で始めてください"))

  def send(d: DeliveryRow): IO[(OutboxResult, Option[Int])] =
    val body = d.payload.noSpaces
    val sig  = "sha256=" + Crypto.hmacHex(d.secret, body.getBytes(StandardCharsets.UTF_8))
    val req = Request[IO](Method.POST, Uri.unsafeFromString(d.url))
      .withEntity(body)
      .withContentType(`Content-Type`(MediaType.application.json))
      .putHeaders("X-Skea-Event" -> d.event, "X-Skea-Delivery" -> d.id.toString, "X-Skea-Signature" -> sig, "User-Agent" -> "skea-webhook")
    // 宛先は送るたびに確かめる（名前解決の結果が変わっても内部へ送らない）
    NetGuard.check(req.uri).flatMap {
      case Left(m) => IO.pure((OutboxResult.Skip(m), None))
      case Right(_) => skea.observe.Telemetry.span("webhook.deliver", "webhook.event" -> d.event)(
        http.run(req).use(r => IO.pure(r.status.code)).timeout(15.seconds).attempt.map {
          case Right(c) if c >= 200 && c < 300 => (OutboxResult.Sent, Some(c))
          case Right(c) if c == 410            => (OutboxResult.Skip(s"HTTP $c"), Some(c))
          case Right(c)                        => (OutboxResult.Retry(s"HTTP $c"), Some(c))
          // 例外の文（内部のホスト名など）は返さない
          case Left(e)                         => (OutboxResult.Retry(s"接続できませんでした（${e.getClass.getSimpleName}）"), None)
        })
    }.flatTap { (r, _) =>
      Integrations.called("webhook", r match
        case OutboxResult.Sent     => "ok"
        case _: OutboxResult.Retry => "retry"
        case _                     => "skip")
    }

  def loop: IO[Nothing] =
    Periodic.run(10.seconds, 10.seconds)(e => s"[skea] webhook worker failed: $e")(integ.processDeliveries(50)(send))

  def purgeLoop: IO[Nothing] =
    Periodic.run(1.hour, 24.hours)(e => s"[skea] webhook purge failed: $e")(integ.purgeDeliveries(30))

/**
 * 通知をチャット（stui）にも送る（docs/stui-integration.md）。連携したユーザーに、stui のボット「skea」からの DM として届く。
 * 通知の種類ごとに個人設定で止められる。トークンが失効していたら（401）連携を外す。
 */
final class ChatNotifier(tokens: IntegrationTokenStore, users: UserStore, http: Client[IO], box: SecretBox, cfg: ApiConfig):

  private val Provider = "stui"

  def send(userId: UUID, kind: String, title: String, text: String, link: String): IO[Unit] =
    cfg.linkServices.find(_.provider == Provider).traverse_ { c =>
      tokens.token(userId, Provider).map(_.flatMap(box.decrypt)).flatMap(_.traverse_ { token =>
        users.chatWanted(userId, kind).flatMap(IO.whenA(_) {
          val req = Request[IO](Method.POST, Uri.unsafeFromString(s"${c.server}/api/apps/notify"))
            .withEntity(Json.obj("title" -> title.asJson, "text" -> text.take(2000).asJson, "url" -> s"${cfg.publicUrl}$link".asJson))
            .putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, token)))
          skea.observe.Telemetry.span("stui.notify", "kind" -> kind)(http.run(req).use { r =>
            Integrations.called(Provider, if r.status.isSuccess then "ok" else "error") *>
              (if r.status == Status.Unauthorized then tokens.deleteToken(userId, Provider)
               else IO.whenA(!r.status.isSuccess)(skea.observe.Log.println(s"[skea] stui notify: ${r.status.code}")))
          }.timeout(10.seconds))
        })
      })
    }.handleErrorWith(e => skea.observe.Log.println(s"[skea] stui notify failed: $e"))

/** ソースコード管理との連携（Webhook の受信・ポーリング）。コミットメッセージのタスクIDでタスクにリンクする。 */
final class ScmService(integ: IntegrationStore, tasks: TaskStore, projects: ProjectStore, hub: EventHub, api: ScmApi, box: SecretBox,
    lock: JobLock):

  /** 受信した Webhook。リンクしたコミットの数を返す。 */
  def receive(repoId: UUID, headers: Headers, body: Array[Byte]): Res[Int] =
    integ.repo(repoId).flatMap {
      case None => IO.pure(Left(Fail.NotFound("リポジトリが見つかりません")))
      case Some(r) =>
        ScmProvider.parse(r.provider) match
          case None => IO.pure(Left(Fail.Invalid("不明なプロバイダです")))
          case Some(p) if !ScmWebhook.verify(p, r.secret, headers, body) => IO.pure(Left(Fail.Unauthorized("署名が正しくありません")))
          case Some(p) if !ScmWebhook.isPush(p, headers) => IO.pure(Right(0))
          case Some(p) =>
            ScmWebhook.commits(p, new String(body, StandardCharsets.UTF_8)) match
              case Left(e)   => IO.pure(Left(Fail.Invalid(e)))
              case Right(cs) => Integrations.called("scm.webhook", "ok") *> linkAll(r, cs).map(Right(_))
    }

  def linkAll(r: RepoRow, commits: List[ScmCommit]): IO[Int] =
    projects.find(r.projectId).flatMap {
      case None => IO.pure(0)
      case Some(p) =>
        // 一括監視で組織名などを指定したときは、その配下のリポジトリだけ
        val owner = Option.when(r.allRepos)(r.name.trim.toLowerCase).filter(_.nonEmpty)
        commits.filter(c => owner.forall(o => c.repo.exists(_.toLowerCase.startsWith(o + "/")))).traverse { c =>
          val keys = TaskKeys.extract(c.message).filter(_.startsWith(p.key + "-"))
          val repo = c.repo.getOrElse(r.name)
          tasks.idsByKeys(p.id, keys).flatMap(_.values.toList.traverse { tid =>
            tasks.linkCommit(tid, r.id, repo, c.sha, c.message, c.url, c.author, c.branch, c.at).flatMap { isNew =>
              if !isNew then IO.pure(0)
              else hub.emitSystem(p.id, Some(tid), "scm", EventKind.CommitLinked, Json.obj(
                "sha" -> c.sha.asJson, "message" -> c.message.take(500).asJson, "url" -> Option(c.url.trim).filter(u => u.startsWith("https://") || u.startsWith("http://")).getOrElse("").asJson, "author" -> c.author.asJson,
                "repo" -> repo.asJson, "branch" -> c.branch.asJson)).as(1)
            }
          }).map(_.sum)
        }.map(_.sum)
    }

  def poll(r: RepoRow): Res[Int] =
    (ScmProvider.parse(r.provider), r.tokenEnc.flatMap(box.decrypt)) match
      case (Some(p), Some(token)) =>
        (NetGuard.check(r.webUrl), r.apiUrl.filter(_.trim.nonEmpty).fold(IO.pure(Right(()): Either[String, Unit]))(NetGuard.check)).tupled.flatMap {
          case (Left(m), _) => IO.pure(Left(Fail.Invalid(m)))
          case (_, Left(m)) => IO.pure(Left(Fail.Invalid(m)))
          case _ => pollWith(r, p, token)
        }
      case (None, _) => IO.pure(Left(Fail.Invalid("不明なプロバイダです")))
      case (_, None) => IO.pure(Left(Fail.Invalid("アクセストークンが設定されていません")))

  private def pollWith(r: RepoRow, p: ScmProvider, token: String): Res[Int] = skea.observe.Telemetry.span("scm.poll", "scm.provider" -> r.provider) {
        val commits =
          if !r.allRepos then api.recentCommits(p, r.webUrl, r.apiUrl, r.name, r.branch, token)
          else
            // 一括監視: 最近更新されたリポジトリごとに取得する（取得できないリポジトリは飛ばす）
            api.recentRepos(p, r.webUrl, r.apiUrl, Option(r.name.trim).filter(_.nonEmpty), token).flatMap(_.flatTraverse(n =>
              api.recentCommits(p, r.webUrl, r.apiUrl, n, None, token).map(_.map(_.copy(repo = Some(n)))).handleError(_ => Nil)))
        commits.attempt.flatMap {
          case Left(e)   =>
            Integrations.called("scm.poll", "error") *> skea.observe.Log.println(s"[skea] scm poll ${r.id} failed: ${e.getMessage}") *>
              IO.pure(Left(Fail.Invalid("コミットを取得できませんでした（URL・トークンを確認してください）")))
          case Right(cs) => Integrations.called("scm.poll", "ok") *> linkAll(r, cs).flatTap(_ => integ.markPolled(r.id, cs.headOption.map(_.sha))).map(Right(_))
        }
  }

  /** トークンを登録したリポジトリを5分ごとに確認する（複数ノードでも1つだけが動く）。 */
  def pollLoop: IO[Nothing] =
    Periodic.run(1.minute, 5.minutes)(e => s"[skea] scm polling failed: $e")(
      lock.exclusive("scm-poll", 4.minutes)(integ.pollableRepos.flatMap(_.traverse_(r => poll(r).void)))
    )
