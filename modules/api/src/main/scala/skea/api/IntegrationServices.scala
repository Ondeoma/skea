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

/**
 * Webhook の送信（アウトボックスのワーカー）。本文に HMAC-SHA256 の署名を付ける。
 * 送信先が stui のチャンネル（kind = stui）なら、本文を文にして stui に投稿する（StuiChannels）。
 */
final class WebhookService(integ: IntegrationStore, http: Client[IO], cfg: ApiConfig, stui: StuiChannels):

  def validUrl(u: String): Either[Fail, String] =
    val t = u.trim
    if t.startsWith("https://") || (cfg.webhookAllowHttp && t.startsWith("http://")) then
      Uri.fromString(t).left.map(_ => Fail.Invalid("URL が正しくありません")).map(_ => t)
    else Left(Fail.Invalid(if cfg.webhookAllowHttp then "URL は http(s):// で始めてください" else "URL は https:// で始めてください"))

  /**
   * 送る要求。本文は payload.noSpaces そのもので、署名もこの本文で計算する。
   * CirceEntityCodec を import しているので、文字のエンコーダーを明示しないと本文が JSON の文字列（"{\"event\":...}"）になる。
   */
  def request(d: DeliveryRow): Request[IO] =
    val body = d.payload.noSpaces
    val sig  = "sha256=" + Crypto.hmacHex(d.secret, body.getBytes(StandardCharsets.UTF_8))
    Request[IO](Method.POST, Uri.unsafeFromString(d.url))
      .withEntity(body)(using EntityEncoder.stringEncoder)
      .withContentType(`Content-Type`(MediaType.application.json))
      .putHeaders("X-Skea-Event" -> d.event, "X-Skea-Delivery" -> d.id.toString, "X-Skea-Signature" -> sig, "User-Agent" -> "skea-webhook")

  def send(d: DeliveryRow): IO[(OutboxResult, Option[Int])] =
    if WebhookKind.isStui(d.kind) then
      val m = ChatMessage.format(d.event, d.payload, cfg.publicUrl)
      skea.observe.Telemetry.span("webhook.deliver", "webhook.event" -> d.event, "webhook.kind" -> d.kind)(
        if d.kind == WebhookKind.StuiHook then stui.postHook(d.url, m) else stui.post(d.ownerId, d.channelId.getOrElse(""), m))
    else sendUrl(d)

  private def sendUrl(d: DeliveryRow): IO[(OutboxResult, Option[Int])] =
    val req = request(d)
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
 * stui のチャンネルへの投稿（プロジェクトの通知. docs/stui-integration.md）。設定した人が stui と OAuth で連携したトークンで、
 * その人が参加しているチャンネルに、stui のボット「skea」として投稿する（POST /api/apps/notify の channel）。
 */
final class StuiChannels(tokens: IntegrationTokenStore, http: Client[IO], box: SecretBox, cfg: ApiConfig):

  private val Provider = "stui"

  private def service: Option[LinkService] = cfg.linkServices.find(_.provider == Provider)

  def configured: Boolean = service.isDefined

  private def token(userId: UUID): IO[Option[String]] = tokens.token(userId, Provider).map(_.flatMap(box.decrypt))

  def connected(userId: UUID): IO[Boolean] = token(userId).map(_.isDefined)

  private def bearer(t: String) = Authorization(Credentials.Token(AuthScheme.Bearer, t))

  /** 投稿先に選べるチャンネル（参加している公開・非公開のもの. アーカイブしたものは除く. admin はチャンネルの管理者）。連携していなければ Left。 */
  def channels(userId: UUID): IO[Either[Fail, List[StuiChannelDto]]] =
    service match
      case None => IO.pure(Left(Fail.NotFound("stui との連携が設定されていません")))
      case Some(c) =>
        token(userId).flatMap {
          case None => IO.pure(Left(Fail.Conflict("stui と連携していません")))
          case Some(t) =>
            val req = Request[IO](Method.GET, Uri.unsafeFromString(s"${c.server}/api/channels")).putHeaders(bearer(t))
            http.run(req).use { r =>
              if r.status == Status.Unauthorized then
                tokens.deleteToken(userId, Provider).as(Left(Fail.Conflict("stui との連携が切れています（連携し直してください）")))
              else if !r.status.isSuccess then IO.pure(Left(Fail.Invalid(s"stui のチャンネルを読めませんでした（HTTP ${r.status.code}）")))
              else r.as[Json].map(j => Right(j.asArray.getOrElse(Vector.empty).toList.flatMap { x =>
                val h = x.hcursor
                for
                  id   <- h.get[String]("id").toOption
                  name <- h.get[String]("name").toOption
                  kind <- h.get[String]("kind").toOption if kind == "public" || kind == "private"
                  if !h.get[Boolean]("archived").getOrElse(false) && h.get[Boolean]("member").getOrElse(true)
                yield StuiChannelDto(id, name, kind, h.get[String]("role").toOption.contains("admin"))
              }.sortBy(_.name.toLowerCase)))
            }.timeout(10.seconds).handleError(_ => Left(Fail.Invalid("stui に接続できませんでした")))
        }

  private def withToken[A](userId: UUID)(f: (LinkService, String) => IO[Either[String, A]]): IO[Either[String, A]] =
    service.fold(IO.pure(Left("stui との連携が設定されていません")))(c =>
      token(userId).flatMap(_.fold(IO.pure(Left("stui と連携してください")))(t => f(c, t))))

  /**
   * 本人の連携で、チャンネルに受信 Webhook を作る（チャンネルの管理者だけが作れる）。(受信 Webhook の ID, 送る URL)。
   * stui が返す URL（{stui の URL}/hooks/{トークン}）から /hooks/{トークン} だけを取り、設定した stui のサーバーの URL（STUI_INTERNAL_URL）に付ける
   * （応答の URL のホストには送らない）。
   */
  private val hookPath = "/hooks/[A-Za-z0-9_=-]+$".r

  def createHook(userId: UUID, channelId: String, name: String): IO[Either[String, (String, String)]] =
    withToken(userId) { (c, t) =>
      val body = Json.obj("channelId" -> channelId.asJson, "name" -> s"skea: $name".take(100).asJson, "username" -> "skea".asJson,
        "iconUrl" -> Json.Null, "enabled" -> true.asJson)
      http.run(Request[IO](Method.POST, Uri.unsafeFromString(s"${c.server}/api/hooks/incoming")).withEntity(body).putHeaders(bearer(t))).use { r =>
        if r.status == Status.Unauthorized then tokens.deleteToken(userId, Provider).as(Left("stui との連携が切れています（連携し直してください）"))
        else if r.status == Status.Forbidden then IO.pure(Left("受信 Webhook を作れるのは、チャンネルの管理者だけです"))
        else if !r.status.isSuccess then IO.pure(Left(s"stui に受信 Webhook を作れませんでした（HTTP ${r.status.code}）"))
        else r.as[Json].map { j =>
          (j.hcursor.downField("hook").get[String]("id"), j.hcursor.get[String]("url")).tupled.toOption
            .flatMap((id, url) => hookPath.findFirstIn(url).map(path => id -> (c.server.stripSuffix("/") + path)))
            .toRight("stui の応答が正しくありません")
        }
      }.timeout(15.seconds).handleError(_ => Left("stui に接続できませんでした"))
    }

  /** 作った受信 Webhook を消す（作った人の連携で. 消せなくても続ける）。 */
  def deleteHook(userId: UUID, hookId: String): IO[Unit] =
    withToken(userId)((c, t) =>
      http.run(Request[IO](Method.DELETE, Uri.unsafeFromString(s"${c.server}/api/hooks/incoming/$hookId")).putHeaders(bearer(t)))
        .use(_ => IO.pure(Right(()))).timeout(10.seconds)).void.handleError(_ => ())

  /** stui の受信 Webhook に送る（トークンは使わない. URL が鍵）。 */
  def postHook(url: String, m: ChatMessage.Message): IO[(OutboxResult, Option[Int])] =
    val text = (if m.url.nonEmpty then s"**[${m.title}](${m.url})**" else s"**${m.title}**") + "\n\n" + m.text.take(2000)
    Uri.fromString(url) match
      case Left(_) => IO.pure((OutboxResult.Skip("受信 Webhook の URL が正しくありません"), None))
      case Right(u) =>
        http.run(Request[IO](Method.POST, u).withEntity(Json.obj("text" -> text.asJson, "username" -> "skea".asJson)))
          .use(r => IO.pure(r.status.code)).timeout(15.seconds).attempt.map {
            case Right(code) if code >= 200 && code < 300 => (OutboxResult.Sent, Some(code))
            case Right(404) => (OutboxResult.Skip("stui の受信 Webhook が見つかりません（消されたかもしれません. 設定し直してください）"), Some(404))
            case Right(403) => (OutboxResult.Skip("このチャンネルには投稿できません（アーカイブされたかもしれません）"), Some(403))
            case Right(code) => (OutboxResult.Retry(s"HTTP $code"), Some(code))
            case Left(e) => (OutboxResult.Retry(s"stui に接続できませんでした（${e.getClass.getSimpleName}）"), None)
          }.flatTap((r, _) => Integrations.called("stui.hook", r match
            case OutboxResult.Sent     => "ok"
            case _: OutboxResult.Retry => "retry"
            case _                     => "skip"))

  /** チャンネルに投稿する（Webhook の送信のワーカーから）。 */
  def post(ownerId: Option[UUID], channelId: String, m: ChatMessage.Message): IO[(OutboxResult, Option[Int])] =
    (service, ownerId) match
      case (None, _) => IO.pure((OutboxResult.Skip("stui との連携が設定されていません（STUI_URL など）"), None))
      case (_, None) => IO.pure((OutboxResult.Skip("設定した人がいません（設定し直してください）"), None))
      case (_, _) if channelId.isEmpty => IO.pure((OutboxResult.Skip("チャンネルが選ばれていません"), None))
      case (Some(c), Some(owner)) =>
        token(owner).flatMap {
          case None => IO.pure((OutboxResult.Skip("設定した人が stui との連携を外しています（設定し直してください）"), None))
          case Some(t) =>
            val req = Request[IO](Method.POST, Uri.unsafeFromString(s"${c.server}/api/apps/notify"))
              .withEntity(Json.obj("title" -> m.title.asJson, "text" -> m.text.take(2000).asJson, "url" -> m.url.asJson, "channel" -> channelId.asJson))
              .putHeaders(bearer(t))
            http.run(req).use(r => IO.pure(r.status.code)).timeout(15.seconds).attempt.flatMap {
              case Right(code) if code >= 200 && code < 300 => IO.pure((OutboxResult.Sent, Some(code)))
              // トークンが失効している: 連携を外す（設定した人が連携し直すまで送れない）
              case Right(401) => tokens.deleteToken(owner, Provider).as((OutboxResult.Skip("stui との連携が切れています（設定した人が連携し直してください）"), Some(401)))
              case Right(403) => IO.pure((OutboxResult.Skip("このチャンネルには投稿できません（設定した人がチャンネルを抜けたかもしれません）"), Some(403)))
              case Right(404) => IO.pure((OutboxResult.Skip("チャンネルが見つかりません"), Some(404)))
              case Right(code) => IO.pure((OutboxResult.Retry(s"HTTP $code"), Some(code)))
              case Left(e) => IO.pure((OutboxResult.Retry(s"stui に接続できませんでした（${e.getClass.getSimpleName}）"), None))
            }
        }.flatTap((r, _) => Integrations.called("stui.channel", r match
          case OutboxResult.Sent     => "ok"
          case _: OutboxResult.Retry => "retry"
          case _                     => "skip"))

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
