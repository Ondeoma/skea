package skea.app

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import doobie.implicits.*
import fs2.io.file.Path
import org.http4s.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.io.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.ErrorHandling
import org.http4s.server.staticcontent.*
import skea.api.*
import skea.auth.{OidcClient, OidcConfig, SecretBox}
import skea.db.*
import skea.integrations.ScmApi
import skea.notify.{SmtpConfig, SmtpMailer}
import skea.shared.ErrorDto
import skea.storage.{S3BlobStore, S3Config}

import scala.concurrent.duration.*

final case class AppConfig(
    port: Int,
    db: DbConfig,
    oidc: Option[OidcConfig],
    api: ApiConfig,
    staticDir: Option[String],
    s3: Option[S3Config],
    smtp: Option[SmtpConfig],
    appSecret: String,
    nodeId: String
)

/** 設定は環境変数から読む。 */
object Config:
  private def opt(name: String): Option[String] = sys.env.get(name).map(_.trim).filter(_.nonEmpty)
  private def int(name: String): Option[Int]    = opt(name).flatMap(_.toIntOption)
  private def enabled(name: String): Boolean    = opt(name).exists(_.equalsIgnoreCase("true"))
  private def required(name: String): Either[String, String] = opt(name).toRight(s"環境変数 $name を設定してください")

  /** OAUTH_CLIENTS: [{"id":"skiwi","secret":"…","redirectUri":"https://…/callback","name":"skiwi"}] */
  private def oauthClients: Either[String, List[skea.api.OAuthClient]] =
    opt("OAUTH_CLIENTS").fold(Right(Nil)) { s =>
      io.circe.parser.parse(s).flatMap(_.as[List[io.circe.Json]]).left.map(_ => "OAUTH_CLIENTS は JSON の配列で指定してください").flatMap(_.traverse { j =>
        val c = j.hcursor
        (c.get[String]("id"), c.get[String]("secret"), c.get[String]("redirectUri")).tupled.toOption
          .filter((id, sec, uri) => id.nonEmpty && sec.length >= 16 && (uri.startsWith("https://") || uri.startsWith("http://localhost")))
          .map((id, sec, uri) => skea.api.OAuthClient(id, sec, uri, c.get[String]("name").getOrElse(id)))
          .toRight("OAUTH_CLIENTS の各要素には id・secret（16文字以上）・redirectUri（https://）を指定してください")
      })
    }

  def load: IO[AppConfig] = IO.fromEither(
    (for
      dbUrl  <- required("DATABASE_URL")
      dbUser <- required("DATABASE_USER")
      dbPass <- required("DATABASE_PASSWORD")
      pub    <- required("PUBLIC_URL")
      devLogin = enabled("DEV_LOGIN")
      oidc <- (opt("OIDC_ISSUER"), opt("OIDC_CLIENT_ID"), opt("OIDC_CLIENT_SECRET")) match
                case (Some(i), Some(c), Some(s)) =>
                  Right(Some(OidcConfig(i, c, s, s"${pub.stripSuffix("/")}/auth/callback", opt("OIDC_GROUPS_CLAIM").getOrElse("groups"))))
                case (None, None, None) if devLogin => Right(None)
                case (None, None, None) => Left("OIDC_ISSUER・OIDC_CLIENT_ID・OIDC_CLIENT_SECRET を設定してください（開発中は DEV_LOGIN=true で省略できます）")
                case _ if devLogin => Left("DEV_LOGIN=true は OIDC を設定していないときだけ使えます（本番では DEV_LOGIN を外してください）")
                case _ => Left("OIDC_ISSUER・OIDC_CLIENT_ID・OIDC_CLIENT_SECRET を設定してください（開発中は DEV_LOGIN=true で省略できます）")
      secret <- opt("APP_SECRET") match
                  case Some(s) if s.toLowerCase.contains("change-me") => Left("APP_SECRET が雛形の値のままです。ランダムな値にしてください")
                  case Some(s) if s.length >= 16 => Right(s)
                  case Some(_) => Left("APP_SECRET は16文字以上にしてください")
                  case None if devLogin => Right("skea-development-secret")
                  case None => Left("APP_SECRET（16文字以上の秘密の値）を設定してください")
      clients <- oauthClients
    yield
      val base = pub.stripSuffix("/")
      AppConfig(
        port = int("HTTP_PORT").getOrElse(8080),
        db = DbConfig(dbUrl, dbUser, dbPass, int("DATABASE_POOL_SIZE").getOrElse(10)),
        oidc = oidc,
        api = ApiConfig(
          adminGroups = opt("ADMIN_GROUPS").map(_.split(',').map(_.trim).filter(_.nonEmpty).toSet).getOrElse(Set.empty),
          cookieSecure = base.startsWith("https://"),
          publicUrl = base,
          devLogin = devLogin,
          maxUploadBytes = int("MAX_UPLOAD_MB").getOrElse(100).toLong * 1024 * 1024,
          timezone = opt("TIMEZONE").getOrElse("Asia/Tokyo"),
          webhookAllowHttp = enabled("WEBHOOK_ALLOW_HTTP"),
          oauthClients = clients,
          // SKIWI_URL・SKIWI_CLIENT_ID・SKIWI_CLIENT_SECRET・SKIWI_INTERNAL_URL（SKAWAU_*・STUI_* も同じ）
          linkServices = skea.api.LinkServiceRoutes.providers.flatMap { p =>
            val e = p.toUpperCase
            (opt(s"${e}_URL"), opt(s"${e}_CLIENT_ID"), opt(s"${e}_CLIENT_SECRET")).mapN((u, id, sec) => skea.api.LinkService(p, u.stripSuffix("/"), id, sec, opt(s"${e}_INTERNAL_URL").map(_.stripSuffix("/"))))
          }
        ),
        staticDir = opt("STATIC_DIR"),
        s3 = opt("S3_ENDPOINT").map(ep => S3Config(ep, opt("S3_REGION").getOrElse("us-east-1"), opt("S3_BUCKET").getOrElse("skea"),
          opt("S3_ACCESS_KEY").getOrElse(""), opt("S3_SECRET_KEY").getOrElse(""), !opt("S3_PATH_STYLE").contains("false"))),
        smtp = opt("SMTP_HOST").map { h =>
          val tls = opt("SMTP_TLS").getOrElse("starttls").toLowerCase
          SmtpConfig(h, int("SMTP_PORT").getOrElse(if tls == "ssl" then 465 else if tls == "none" then 25 else 587), opt("SMTP_USER"),
            opt("SMTP_PASSWORD"), tls, opt("MAIL_FROM").getOrElse(s"skea@$h"))
        },
        appSecret = secret,
        nodeId = opt("NODE_ID").getOrElse(java.net.InetAddress.getLocalHost.getHostName + "-" + java.util.UUID.randomUUID().toString.take(8))
      )
    ).left.map(new IllegalArgumentException(_))
  )

object Main extends IOApp.Simple:

  private given org.typelevel.log4cats.LoggerFactory[IO] = org.typelevel.log4cats.slf4j.Slf4jFactory.create[IO]

  def run: IO[Unit] =
    for
      cfg <- Config.load
      n   <- migrateWithRetry(cfg)
      _   <- IO.println(s"[skea] database migrated ($n migration(s) applied); node ${cfg.nodeId}")
      _ <- (for
        xa    <- Database.transactor(cfg.db)
        http  <- EmberClientBuilder.default[IO].withTimeout(30.seconds).build
        users    = new UserStore(xa)
        projects = new ProjectStore(xa)
        tasks    = new TaskStore(xa)
        activity = new ActivityStore(xa)
        integ    = new IntegrationStore(xa)
        search   = new SearchStore(xa, tasks)
        lock     = new JobLock(xa, cfg.nodeId)
        box      = new SecretBox(cfg.appSecret)
        oidc  <- Resource.eval(cfg.oidc.traverse(OidcClient.create(_, http)))
        blobs <- blobStore(cfg)
        sess     = new SessionAuth(users, cfg.api)
        access   = new Access2(projects, tasks)
        hub      = new EventHub(tasks, projects, integ)
        tokens   = new IntegrationTokenStore(xa)
        notifier = new NotifyService(activity, projects, users, sess, cfg.api, Some(new ChatNotifier(tokens, users, http, box, cfg.api)))
        svc      = new TaskService(tasks, projects, access, hub, notifier, cfg.api.adminGroups)
        hooks    = new WebhookService(integ, http, cfg.api)
        engine   = new AutomationEngine(integ, tasks, projects, users, activity, search, sess, access, svc, notifier, http, lock, cfg.api)
        scm      = new ScmService(integ, tasks, projects, hub, new ScmApi(http), box, lock)
        mailer   = cfg.smtp.map(new SmtpMailer(_))
        tplStore = new TemplateStore(xa)
        emojis   = new EmojiStore(xa)
        tplSvc   = new TemplateService(tplStore, projects, svc, sess, lock, cfg.api)
        imports  = new ImportService(projects, users, svc, cfg.api)
        topic   <- Resource.eval(fs2.concurrent.Topic[IO, skea.api.FeedItem])
        feed     = new EventFeed(tasks, topic)
        cache   <- Resource.eval(LinkServiceRoutes.newCache)
        _ <- Resource.eval(startupNotes(cfg))
        loops = List[IO[Unit]](
                  notifier.mailLoop(mailer), hooks.loop, hooks.purgeLoop, engine.loop, engine.scheduledLoop, engine.purgeLoop,
                  scm.pollLoop, tplSvc.loop, feed.loop, dueLoop(tasks, activity, notifier, lock, cfg), purgeLoop(users, activity, lock)
                )
        _ <- loops.traverse_(_.background)
        rest =
          healthRoutes(xa, cfg.nodeId) <+>
            new AuthRoutes(oidc, users, activity, sess, access, cfg.api).routes <+>
            new UserRoutes(users, projects, activity, blobs, emojis, sess, access).routes <+>
            new ProjectRoutes(projects, tasks, users, activity, svc, hub, sess, access, cfg.api).routes <+>
            new TaskRoutes(tasks, users, activity, integ, svc, hub, engine, blobs, emojis, sess, access, cfg.api).routes <+>
            new SearchRoutes(search, projects, activity, sess, access).routes <+>
            new IntegrationRoutes(integ, projects, users, activity, hooks, engine, scm, box, sess, access, cfg.api).routes <+>
            new TemplateRoutes(tplStore, tplSvc, imports, sess, access).routes <+>
            new StreamRoutes(feed, sess, access).routes <+>
            new OAuthRoutes(new OAuthStore(xa), tasks, projects, activity, sess, access, cfg.api).routes <+>
            new LinkServiceRoutes(tokens, http, box, cache, sess, access, cfg.api).routes
        app = rest <+> new McpRoutes(rest, sess, cfg.api.publicUrl).routes
        all = cfg.staticDir.fold(app)(d => app <+> spa(d))
        _ <- EmberServerBuilder.default[IO]
               .withHost(ipv4"0.0.0.0")
               .withPort(Port.fromInt(cfg.port).getOrElse(port"8080"))
               .withShutdownTimeout(5.seconds)
               .withHttpApp(ErrorHandling.Custom.recoverWith(secured(all, cfg.api.maxUploadBytes).orNotFound)(handler))
               .build
        _ <- Resource.eval(IO.println(s"[skea] listening on :${cfg.port} (${cfg.api.publicUrl})"))
      yield ()).useForever
    yield ()

  /** DB（Patroni のプライマリ）が使えるまで待ってからマイグレーションする。複数ノード同時起動でも Flyway のロックで1回だけ適用される。 */
  private def migrateWithRetry(cfg: AppConfig, attempt: Int = 1): IO[Int] =
    Database.migrate(cfg.db).handleErrorWith { e =>
      if attempt >= 60 then IO.raiseError(e)
      else IO.println(s"[skea] database not ready (${e.getMessage.take(120)}); retry $attempt/60") *> IO.sleep(2.seconds) *> migrateWithRetry(cfg, attempt + 1)
    }

  /** ロードバランサー用のヘルスチェック。DB（プライマリ）に書けない状態なら 503。 */
  private def healthRoutes(xa: doobie.Transactor[IO], node: String): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "healthz" =>
      sql"select not pg_is_in_recovery()".query[Boolean].unique.transact(xa).timeout(3.seconds).attempt.flatMap {
        case Right(true)  => text(Status.Ok, s"ok $node")
        case Right(false) => text(Status.ServiceUnavailable, "database is read-only (not primary)")
        case Left(e)      => IO.println(s"[skea] healthz: $e") *> text(Status.ServiceUnavailable, "database unavailable")
      }
  }

  private def text(status: Status, body: String): IO[Response[IO]] =
    IO.pure(Response[IO](status).withEntity(body)(using EntityEncoder.stringEncoder))

  private def blobStore(cfg: AppConfig): Resource[IO, Option[skea.storage.BlobStore]] = cfg.s3 match
    case Some(c) => S3BlobStore.resource(c).evalTap(_.ensureBucket).map(Some(_))
    case None    => Resource.eval(IO.println("[skea] WARNING: S3_ENDPOINT が未設定のため、添付ファイルは使えません").as(None))

  private def startupNotes(cfg: AppConfig): IO[Unit] =
    IO.whenA(cfg.smtp.isEmpty)(IO.println("[skea] WARNING: SMTP_HOST が未設定のため、メールは送りません")) *>
      IO.whenA(cfg.oidc.isEmpty)(IO.println("[skea] WARNING: OIDC が未設定です。開発用のログイン（DEV_LOGIN）を使います。本番では使わないでください"))

  private val handler: PartialFunction[Throwable, IO[Response[IO]]] = { case e =>
    IO.println(s"[skea] unhandled error: $e") *> InternalServerError(ErrorDto("サーバーでエラーが発生しました"))
  }

  /** 期日が近い・過ぎたタスクを担当者に1日1回通知する（1時間ごとに確認）。 */
  private def dueLoop(tasks: TaskStore, activity: ActivityStore, notifier: NotifyService, lock: JobLock, cfg: AppConfig): IO[Unit] =
    Periodic.run(2.minutes, 1.hour)(e => s"[skea] due check failed: $e")(lock.exclusive("due-check", 50.minutes) {
      val zone  = Op.zone(cfg.api.timezone)
      val today = java.time.LocalDate.now(zone)
      val since = today.atStartOfDay(zone).toInstant
      tasks.dueSoon(today).flatMap(_.traverse_ { (tid, pid, key, title, due, assignee) =>
        activity.dueNotified(assignee, tid, since).ifM(IO.unit, {
          val when = if due.isBefore(today) then "期日を過ぎています" else if due == today then "今日が期日です" else "明日が期日です"
          notifier.send(skea.shared.NotificationKind.Due, Set(assignee), None, pid, Some(tid), None, s"$key: $when", title, s"/tasks/$key")
        })
      })
    })

  private def purgeLoop(users: UserStore, activity: ActivityStore, lock: JobLock): IO[Unit] =
    Periodic.run(10.minutes, 1.hour)(e => s"[skea] purge failed: $e")(lock.exclusive("purge", 50.minutes)(
      IO.realTimeInstant.flatMap(users.purgeExpired) *> activity.purgeOld(180).void))

  /** すべての応答に付けるセキュリティ用のヘッダー（個別に付けたものは上書きしない）と、リクエスト本文の大きさの上限
    * （ファイルのアップロード（multipart）は MAX_UPLOAD_MB（と multipart の見出しの分の余裕 1MB）、受信 Webhook は 10MB、それ以外は 2MB）。 */
  private def secured(routes: HttpRoutes[IO], maxUpload: Long): HttpRoutes[IO] =
    val defaults = List("X-Frame-Options" -> "DENY", "X-Content-Type-Options" -> "nosniff", "Referrer-Policy" -> "same-origin",
      "Content-Security-Policy" -> "frame-ancestors 'none'; object-src 'none'; base-uri 'self'")
    def limitOf(req: Request[IO]): Long =
      if req.contentType.exists(_.mediaType.isMultipart) then maxUpload + 1024 * 1024
      else if req.pathInfo.renderString.startsWith("/hooks/") then 10L * 1024 * 1024
      else 2L * 1024 * 1024
    HttpRoutes[IO] { req =>
      val limit = limitOf(req)
      val res =
        if req.contentLength.exists(_ > limit) then cats.data.OptionT.some[IO](Response[IO](Status.PayloadTooLarge).withEntity(ErrorDto(
          if limit >= maxUpload then s"ファイルが大きすぎます（上限 ${maxUpload / 1024 / 1024}MB）" else "送信する内容が大きすぎます")))
        else
          // Content-Length がない（chunked）ときも、上限を超えたら読むのをやめる
          val body = req.body.chunks.mapAccumulate(0L)((n, c) => (n + c.size, c)).flatMap { (n, c) =>
            if n > limit then fs2.Stream.raiseError[IO](new IllegalArgumentException("request body too large")) else fs2.Stream.chunk(c)
          }
          routes(req.withBodyStream(body))
      res.map(r => defaults.foldLeft(r)((acc, h) => if acc.headers.get(org.typelevel.ci.CIString(h._1)).isDefined then acc else acc.putHeaders(h)))
    }

  /** ビルド済みの画面の配信。存在しないパスは index.html（SPA）。 */
  private def spa(dir: String): HttpRoutes[IO] =
    val files = fileService[IO](FileService.Config(dir))
    val index = HttpRoutes.of[IO] {
      case req @ GET -> _ if !Set("api", "auth", "hooks", "mcp", "oauth").exists(p => req.pathInfo.renderString.startsWith("/" + p)) =>
        StaticFile.fromPath(Path(dir) / "index.html", Some(req)).getOrElseF(NotFound())
    }
    HttpRoutes[IO] { req =>
      val cc = if req.pathInfo.renderString.startsWith("/assets/") then "public, max-age=31536000, immutable" else "no-cache"
      (files <+> index).run(req).map(_.putHeaders("Cache-Control" -> cc))
    }
