package skea.app

import sbirds.links.IntegrationTokenStore

import skea.core.ErrorCode

import sbirds.observe.Log

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import doobie.implicits.*
import fs2.io.file.Path
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.staticcontent.*
import skea.api.*
import sbirds.auth.{OidcClient, OidcConfig}
import skea.auth.SecretBox
import skea.db.*
import skea.integrations.ScmApi
import sbirds.mail.{SmtpConfig, SmtpMailer}
import sbirds.observe.{HttpClientTelemetry, HttpTelemetry, Metrics, RumRoutes, Telemetry}
import sbirds.http.Errors
import sbirds.storage.{S3BlobStore, S3Config}

import scala.concurrent.duration.*

final case class AppConfig(port: Int,
                           db: DbConfig,
                           oidc: Option[OidcConfig],
                           api: ApiConfig,
                           staticDir: Option[String],
                           s3: Option[S3Config],
                           smtp: Option[SmtpConfig],
                           appSecret: String,
                           nodeId: String)

object Main extends IOApp.Simple {

  // ログにトレースの ID・操作したユーザーを載せるため、IOLocal の文脈を Java のライブラリから見えるようにする
  // （IORuntime を作る前に設定する必要がある. オブジェクトの初期化は main より先に動く）
  Telemetry.prepare()

  private val version =
    Option(getClass.getPackage.getImplementationVersion).orElse(sys.env.get("APP_VERSION")).getOrElse("dev")

  private given org.typelevel.log4cats.LoggerFactory[IO] = org.typelevel.log4cats.slf4j.Slf4jFactory.create[IO]

  def run: IO[Unit] =
    for {
      cfg <- Config.load
      n <- migrateWithRetry(cfg)
      _ <- sbirds.observe.Log.println(s"[skea] database migrated ($n migration(s) applied); node ${cfg.nodeId}")
      _ <- (for {
        _ <- Telemetry.resource("skea", version, runtime)
        xa <- Database.transactor(cfg.db)
        _ <- gauges(xa)
        // 外への呼び出しもトレースに出す（traceparent を付けて送る）
        http <- EmberClientBuilder.default[IO].withTimeout(30.seconds).build.map(HttpClientTelemetry(_))
        users = new UserStore(xa)
        projects = new ProjectStore(xa)
        tasks = new TaskStore(xa)
        activity = new ActivityStore(xa)
        integ = new IntegrationStore(xa)
        search = new SearchStore(xa, tasks)
        lock = new JobLock(xa, cfg.nodeId)
        box = SecretBox(cfg.appSecret)
        oidc <- Resource.eval(cfg.oidc.traverse(OidcClient.create(_, http)))
        blobs <- blobStore(cfg)
        sess = new SessionAuth(users, cfg.api)
        access = new Access2(projects, tasks)
        hub = new EventHub(tasks, projects, integ)
        tokens = new IntegrationTokenStore(xa)
        pushStore = new sbirds.push.PushStore(xa)
        push <- Resource.eval(sbirds.push.PushService.create(pushStore, http, cfg.api.publicUrl, "skea"))
        help = new HelpStore(xa)
        notifier = new NotifyService(
          activity,
          projects,
          users,
          help,
          sess,
          cfg.api,
          Some(new ChatNotifier(tokens, users, http, box, cfg.api)),
          Some(push)
        )
        svc = new TaskService(tasks, projects, access, hub, notifier, cfg.api.adminGroups)
        stuiCh = new StuiChannels(tokens, http, box, cfg.api)
        hooks = new WebhookService(integ, http, cfg.api, stuiCh)
        engine = new AutomationEngine(
          integ,
          tasks,
          projects,
          users,
          activity,
          search,
          sess,
          access,
          svc,
          notifier,
          http,
          lock,
          cfg.api
        )
        scm = new ScmService(integ, tasks, projects, hub, new ScmApi(http), box, lock)
        mailer = cfg.smtp.map(new SmtpMailer(_))
        tplStore = new TemplateStore(xa)
        emojis = new EmojiStore(xa)
        tplSvc = new TemplateService(tplStore, projects, svc, sess, lock, cfg.api)
        imports = new ImportService(projects, users, svc, cfg.api)
        files = new Attachments(tasks, users, hub, blobs)
        topic <- Resource.eval(fs2.concurrent.Topic[IO, skea.api.FeedItem])
        feed = new EventFeed(tasks, topic)
        cache <- Resource.eval(LinkServiceRoutes.newCache)
        _ <- Resource.eval(startupNotes(cfg))
        loops = List[IO[Unit]](
          notifier.mailLoop(mailer),
          hooks.loop,
          hooks.purgeLoop,
          engine.loop,
          engine.scheduledLoop,
          engine.purgeLoop,
          scm.pollLoop,
          tplSvc.loop,
          feed.loop,
          dueLoop(tasks, activity, notifier, lock, cfg),
          purgeLoop(users, activity, lock)
        )
        _ <- loops.traverse_(_.background)
        rest =
          healthRoutes(xa, cfg.nodeId) <+>
            new AuthRoutes(oidc, users, activity, sess, access, cfg.api).routes <+>
            new UserRoutes(users, projects, activity, blobs, emojis, sess, access).routes <+>
            new ProjectRoutes(projects, tasks, users, activity, svc, hub, sess, access, cfg.api).routes <+>
            new TaskRoutes(
              tasks,
              users,
              activity,
              integ,
              svc,
              hub,
              engine,
              blobs,
              files,
              emojis,
              sess,
              access,
              cfg.api
            ).routes <+>
            new HelpRoutes(help, projects, tasks, activity, svc, hub, notifier, files, sess, access, cfg.api).routes <+>
            new SearchRoutes(search, projects, activity, sess, access).routes <+>
            new IntegrationRoutes(
              integ,
              projects,
              users,
              activity,
              hooks,
              stuiCh,
              engine,
              scm,
              box,
              sess,
              access,
              cfg.api
            ).routes <+>
            new TemplateRoutes(tplStore, tplSvc, imports, sess, access).routes <+>
            new StreamRoutes(feed, sess, access).routes <+>
            new OAuthRoutes(new OAuthStore(xa), tasks, projects, activity, sess, access, cfg.api).routes <+>
            new LinkServiceRoutes(tokens, http, box, cache, sess, access, cfg.api).routes <+>
            RumRoutes.routes <+>
            new PushRoutes(pushStore, push, sess, access).routes
        app = rest <+> new McpRoutes(rest, sess, cfg.api.publicUrl).routes
        all = cfg.staticDir.fold(app)(d => app <+> spa(d))
        httpApp <- HttpTelemetry(
          Errors.handle(secured(all, cfg.api.maxUploadBytes).orNotFound, ErrorCode.Unexpected),
          apiPaths.map("/" + _)
        )
        _ <- EmberServerBuilder.default[IO]
          .withHost(ipv4"0.0.0.0")
          .withPort(Port.fromInt(cfg.port).getOrElse(port"8080"))
          .withShutdownTimeout(5.seconds)
          .withHttpApp(httpApp)
          .build
        _ <- Resource.eval(sbirds.observe.Log.println(s"[skea] listening on :${cfg.port} (${cfg.api.publicUrl})"))
      } yield ()).useForever
    } yield ()

  /** DB（Patroni のプライマリ）が使えるまで待ってからマイグレーションする。複数ノード同時起動でも Flyway のロックで1回だけ適用される。 */
  private def migrateWithRetry(cfg: AppConfig, attempt: Int = 1): IO[Int] =
    Database.migrate(cfg.db).handleErrorWith { e =>
      if attempt >= 60 then IO.raiseError(e)
      else
        Log.report(ErrorCode.DatabaseNotReady, s"${e.getMessage.take(120)}; retry $attempt/60") *>
          IO.sleep(2.seconds) *> migrateWithRetry(cfg, attempt + 1)
    }

  /** DB から定期的に読むメトリクス（skorimako のダッシュボード・アラート用）。 */
  private def gauges(xa: doobie.Transactor[IO]): Resource[IO, Unit] = {
    def q(sqlText: doobie.Fragment): IO[Long] = sqlText.query[Long].unique.transact(xa)
    // 未完了のタスク（削除・アーカイブしたプロジェクトのものは除く）
    val open = fr"""from task t join status s on s.id = t.status_id join project p on p.id = t.project_id
                    where t.deleted_at is null and not p.archived and s.category <> 'done'"""
    List(
      Metrics.gauge("skea.users.total", "有効なユーザーの数")(q(sql"select count(*) from app_user where not disabled")),
      Metrics.gauge(
        "skea.users.active_24h",
        "24時間以内にログインしたユーザーの数"
      )(q(sql"select count(*) from app_user where last_login_at > now() - interval '24 hours'")),
      Metrics.gauge(
        "skea.sessions.active",
        "有効なセッションの数"
      )(q(sql"select count(*) from session where expires_at > now()")),
      Metrics.gauge("skea.tasks.open", "未完了のタスクの数")(q(fr"select count(*)" ++ open)),
      Metrics.gauge("skea.tasks.overdue", "期日を過ぎた未完了のタスクの数")(q(fr"select count(*)" ++ open ++
        fr"and t.due_date < current_date")),
      Metrics.gauge("skea.sprints.active", "進行中のスプリントの数")(q(
        sql"select count(*) from sprint s join project p on p.id = s.project_id where s.state = 'active' and not p.archived"
      )),
      Metrics.gauge(
        "skea.mail.outbox",
        "送信待ちのメールの数"
      )(q(sql"select count(*) from mail_outbox where sent_at is null and not skipped")),
      Metrics.gauge(
        "skea.webhook.pending",
        "送信待ちの Webhook の数"
      )(q(sql"select count(*) from webhook_delivery where done_at is null and not failed"))
    ).sequence_
  }

  /** ロードバランサー用のヘルスチェック。DB（プライマリ）に書けない状態なら 503。 */
  private def healthRoutes(xa: doobie.Transactor[IO], node: String): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "healthz" =>
      sql"select not pg_is_in_recovery()".query[Boolean].unique.transact(xa).timeout(3.seconds).attempt.flatMap {
        case Right(true) => text(Status.Ok, s"ok $node")
        case Right(false) => text(Status.ServiceUnavailable, "database is read-only (not primary)")
        case Left(e) => Log.report(ErrorCode.HealthCheckFailed, "", e) *>
            text(Status.ServiceUnavailable, "database unavailable")
      }
  }

  private def text(status: Status, body: String): IO[Response[IO]] =
    IO.pure(Response[IO](status).withEntity(body)(using EntityEncoder.stringEncoder))

  private def blobStore(cfg: AppConfig): Resource[IO, Option[sbirds.storage.BlobStore]] = cfg.s3 match {
    case Some(c) => S3BlobStore.resource(c).evalTap(_.ensureBucket).map(Some(_))
    case None => Resource.eval(sbirds.observe.Log.println("[skea] WARNING: S3_ENDPOINT が未設定のため、添付ファイルは使えません").as(None))
  }

  private def startupNotes(cfg: AppConfig): IO[Unit] =
    IO.whenA(cfg.smtp.isEmpty)(sbirds.observe.Log.println("[skea] WARNING: SMTP_HOST が未設定のため、メールは送りません")) *>
      IO.whenA(
        cfg.oidc.isEmpty
      )(sbirds.observe.Log.println("[skea] WARNING: OIDC が未設定です。開発用のログイン（DEV_LOGIN）を使います。本番では使わないでください"))

  /** 期日が近い・過ぎたタスクを担当者に1日1回通知する（1時間ごとに確認）。 */
  private def dueLoop(tasks: TaskStore,
                      activity: ActivityStore,
                      notifier: NotifyService,
                      lock: JobLock,
                      cfg: AppConfig): IO[Unit] =
    Periodic.run(2.minutes, 1.hour, ErrorCode.DueCheckFailed)(lock.exclusive("due-check", 50.minutes) {
      val zone = Op.zone(cfg.api.timezone)
      val today = java.time.LocalDate.now(zone)
      val since = today.atStartOfDay(zone).toInstant
      tasks.dueSoon(today).flatMap(_.traverse_ {
        (tid, pid, key, title, due, assignee) =>
          activity.dueNotified(assignee, tid, since).ifM(
            IO.unit, {
              val when = if due.isBefore(today) then "期日を過ぎています" else if due == today then "今日が期日です" else "明日が期日です"
              notifier.send(
                skea.shared.NotificationKind.Due,
                Set(assignee),
                None,
                pid,
                Some(tid),
                None,
                s"$key: $when",
                title,
                s"/tasks/$key"
              )
            }
          )
      })
    })

  private def purgeLoop(users: UserStore, activity: ActivityStore, lock: JobLock): IO[Unit] =
    Periodic.run(10.minutes, 1.hour, ErrorCode.PurgeFailed)(lock.exclusive("purge", 50.minutes)(
      IO.realTimeInstant.flatMap(users.purgeExpired) *> activity.purgeOld(180).void
    ))

  /**
   * すべての応答に付けるセキュリティ用のヘッダー（個別に付けたものは上書きしない）と、リクエスト本文の大きさの上限
   * （ファイルのアップロード（multipart）は MAX_UPLOAD_MB（と multipart の見出しの分の余裕 1MB）、受信 Webhook は 10MB、それ以外は 2MB）。
   */
  private def secured(routes: HttpRoutes[IO], maxUpload: Long): HttpRoutes[IO] = {
    val defaults = List(
      "X-Frame-Options" -> "DENY",
      "X-Content-Type-Options" -> "nosniff",
      "Referrer-Policy" -> "same-origin",
      "Content-Security-Policy" -> "frame-ancestors 'none'; object-src 'none'; base-uri 'self'"
    )
    def limitOf(req: Request[IO]): Long =
      if req.contentType.exists(_.mediaType.isMultipart) then maxUpload + 1024 * 1024
      else if req.pathInfo.renderString.startsWith("/hooks/") then 10L * 1024 * 1024
      else 2L * 1024 * 1024
    HttpRoutes[IO] { req =>
      val limit = limitOf(req)
      val res =
        if req.contentLength.exists(_ > limit) then
          cats.data.OptionT.liftF(
            if limit >= maxUpload then
              Errors.respond(ErrorCode.UploadTooLarge, s"ファイルが大きすぎます（上限 ${maxUpload / 1024 / 1024}MB）")
            else Errors.respond(ErrorCode.RequestTooLarge)
          )
        else {
          // Content-Length がない（chunked）ときも、上限を超えたら読むのをやめる
          val body = req.body.chunks.mapAccumulate(0L)((n, c) => (n + c.size, c)).flatMap {
            (n, c) =>
              if n > limit then fs2.Stream.raiseError[IO](new IllegalArgumentException("request body too large"))
              else fs2.Stream.chunk(c)
          }
          routes(req.withBodyStream(body))
        }
      res.map(r =>
        defaults.foldLeft(r)((acc, h) =>
          if acc.headers.get(org.typelevel.ci.CIString(h._1)).isDefined then acc else acc.putHeaders(h)
        )
      )
    }
  }

  /** バックエンドのパスの先頭（画面の index.html を返さない. トレース・アクセスログの対象）。 */
  private val apiPaths = List("api", "auth", "hooks", "mcp", "oauth")

  /** ビルド済みの画面の配信。存在しないパスは index.html（SPA）。 */
  private def spa(dir: String): HttpRoutes[IO] = {
    val files = fileService[IO](FileService.Config(dir))
    val index = HttpRoutes.of[IO] {
      case req @ GET -> _ if !apiPaths.exists(p => req.pathInfo.renderString.startsWith("/" + p)) =>
        StaticFile.fromPath(Path(dir) / "index.html", Some(req)).getOrElseF(NotFound())
    }
    HttpRoutes[IO] { req =>
      val cc =
        if req.pathInfo.renderString.startsWith("/assets/") then "public, max-age=31536000, immutable" else "no-cache"
      (files <+> index).run(req).map(_.putHeaders("Cache-Control" -> cc))
    }
  }
}
