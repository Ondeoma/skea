package skea.api

import sbirds.db.*
import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import munit.CatsEffectSuite
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.headers.Authorization
import org.http4s.implicits.*
import skea.core.*
import skea.db.*

import java.util.UUID

/**
 * DB を使う API のテストの共通部分（DB の用意・本番と同じ組み立てのルート・リクエストの送り方）。
 * TEST_DATABASE_URL がなければ飛ばす（例: jdbc:postgresql://localhost:54339/skea_test）。
 */
abstract class ApiDbSuite extends CatsEffectSuite {

  protected val cfg: Option[DbConfig] = sys.env.get("TEST_DATABASE_URL").map(url =>
    DbConfig(url, sys.env.getOrElse("TEST_DATABASE_USER", "skea"), sys.env.getOrElse("TEST_DATABASE_PASSWORD", "skea"))
  )

  protected val xaFixture = ResourceSuiteLocalFixture(
    "xa",
    cfg.fold(Resource.eval(IO.raiseError[doobie.Transactor[IO]](new RuntimeException("no db"))))(c =>
      Resource.eval(Database.migrate(c)) *> Database.transactor(c)
    )
  )

  override def munitFixtures = if cfg.isDefined then List(xaFixture) else Nil

  protected val publicUrl = "http://localhost:8090"
  protected def apiCfg: ApiConfig = ApiConfig(adminGroups = Set.empty, cookieSecure = false, publicUrl = publicUrl)

  /** 本番と同じ組み立てのルート（外部への送信はしない）。 */
  protected final class Env(xa: doobie.Transactor[IO]) {
    val users = new UserStore(xa)
    val projects = new ProjectStore(xa)
    val tasks = new TaskStore(xa)
    val search = new SearchStore(xa, tasks)
    val activity = new ActivityStore(xa)
    val integ = new IntegrationStore(xa)
    val help = new HelpStore(xa)
    val sess = new SessionAuth(users, apiCfg)
    val access = new Access2(projects, tasks)
    val hub = new EventHub(tasks, projects, integ)
    val notifier = new NotifyService(activity, projects, users, help, sess, apiCfg)
    val svc = new TaskService(tasks, projects, access, hub, notifier, Set.empty)
    val blobs = new HelpdeskApiSpec.MemoryBlobs
    val files = new Attachments(tasks, users, hub, Some(blobs))
    val emoji = new EmojiStore(xa)
    val engine = new AutomationEngine(
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
      Client.fromHttpApp(HttpApp.notFound[IO]),
      new JobLock(xa, "test"),
      apiCfg
    )
    val rest: HttpRoutes[IO] =
      new ProjectRoutes(projects, tasks, users, activity, svc, hub, sess, access, apiCfg).routes <+>
        new TaskRoutes(
          tasks,
          users,
          activity,
          integ,
          svc,
          hub,
          engine,
          Some(blobs),
          files,
          emoji,
          sess,
          access,
          apiCfg
        ).routes <+>
        new HelpRoutes(help, projects, tasks, activity, svc, hub, notifier, files, sess, access, apiCfg).routes <+>
        new SearchRoutes(search, projects, activity, sess, access).routes <+>
        new UserRoutes(users, projects, activity, Some(blobs), emoji, sess, access).routes
    val app: HttpApp[IO] = (rest <+> new McpRoutes(rest, sess, publicUrl).routes).orNotFound

    def user(name: String, email: Option[String] = None): IO[(User, String)] =
      users.upsert(
        s"test:${UUID.randomUUID()}",
        name,
        Some(email.getOrElse(s"${UUID.randomUUID().toString.take(8)}@example.com")),
        None
      )
        .flatMap(u => sess.issue(u).map(c => (u, c.content)))

    /** 送って、状態と JSON の本文を返す。 */
    def send(req: Request[IO]): IO[(Status, Json)] =
      app.run(req).flatMap(r =>
        r.bodyText.compile.string.map(b => (r.status, io.circe.parser.parse(b).getOrElse(Json.Null)))
      )

    /** 画面（Cookie のセッション）から送る。 */
    def call(token: String, method: Method, path: String, body: Option[Json] = None): IO[(Status, Json)] = {
      val base = Request[IO](method, Uri.unsafeFromString(path)).addCookie("skea_session", token).putHeaders("Origin" ->
        publicUrl)
      send(body.fold(base)(base.withEntity(_)))
    }

    /** APIキー（Bearer）で送る。 */
    def callKey(key: String, method: Method, path: String, body: Option[Json] = None): IO[(Status, Json)] = {
      val base = Request[IO](
        method,
        Uri.unsafeFromString(path)
      ).putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, key)))
      send(body.fold(base)(base.withEntity(_)))
    }

    def get(token: String, path: String) = call(token, Method.GET, path)

    /** ファイルを multipart/form-data（file）で送る。 */
    def upload(token: String, path: String, name: String, ct: MediaType, bytes: Array[Byte]): IO[(Status, Json)] =
      org.http4s.multipart.Multiparts.forSync[IO].flatMap(_.multipart(Vector(
        org.http4s.multipart.Part.fileData[IO](
          "file",
          name,
          Entity.stream(fs2.Stream.emits(bytes.toSeq).covary[IO], Some(bytes.length.toLong)),
          org.http4s.headers.`Content-Type`(ct)
        )
      ))).flatMap { mp =>
        send(Request[IO](
          Method.POST,
          Uri.unsafeFromString(path)
        ).addCookie("skea_session", token).putHeaders("Origin" -> publicUrl)
          .withEntity(mp).putHeaders(mp.headers))
      }
    def post(token: String, path: String, body: Json) = call(token, Method.POST, path, Some(body))
    def put(token: String, path: String, body: Json) = call(token, Method.PUT, path, Some(body))
  }

  protected def withEnv(name: String)(body: Env => IO[Unit]): Unit =
    test(name) {
      if cfg.isEmpty then IO(assume(false, "TEST_DATABASE_URL が未設定")) else body(new Env(xaFixture()))
    }
}
