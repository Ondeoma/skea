package skea.api

import cats.effect.*
import io.circe.Json
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.{Authorization, Cookie, Location}
import org.http4s.implicits.*
import sbirds.auth.Crypto
import skea.db.*

import java.time.Instant

/**
 * OAuth（認可コード + PKCE）: クライアントに許したスコープだけを発行する（skakapo のように write を許したクライアントは読み書きの API キーになる）。
 * DB を使う（ApiDbSuite）。
 */
class OAuthSpec extends ApiDbSuite {

  private val clients = List(
    OAuthClient("skiwi", "skiwi-secret-0123456789", "http://localhost:8080/cb", "skiwi"),
    OAuthClient(
      "skakapo",
      "skakapo-secret-0123456789",
      "http://localhost:5199/api/connections/callback",
      "skakapo",
      List("read", "write")
    )
  )
  override protected def apiCfg: ApiConfig = super.apiCfg.copy(oauthClients = clients)

  /** 許可して認可コードを受け取り、トークンに換えて、発行された API キーのスコープを返す. */
  private def grant(clientId: String,
                    secret: String,
                    redirect: String,
                    scope: String): IO[Either[String, List[String]]] = {
    val xa = xaFixture()
    val users = new UserStore(xa); val projects = new ProjectStore(xa); val tasks = new TaskStore(xa)
    val sess = new SessionAuth(users, apiCfg)
    val routes = new OAuthRoutes(
      new OAuthStore(xa),
      tasks,
      projects,
      new ActivityStore(xa),
      sess,
      new Access2(projects, tasks),
      apiCfg
    ).routes.orNotFound
    val verifier = Crypto.randomToken(48)
    val q =
      s"client_id=$clientId&redirect_uri=${java.net.URLEncoder.encode(redirect, "UTF-8")}&response_type=code&state=st" +
        s"&code_challenge=${Crypto.pkceChallenge(verifier)}&code_challenge_method=S256&scope=${java.net.URLEncoder.encode(scope, "UTF-8")}"
    for {
      u <- users.upsert(s"oauth-${java.util.UUID.randomUUID()}", "連携 太郎", None, None)
      ck <- sess.issue(u)
      page <- routes.run(Request[IO](
        Method.GET,
        Uri.unsafeFromString(s"/oauth/authorize?$q")
      ).putHeaders(Cookie(RequestCookie(ck.name, ck.content))))
      res <- page.status match {
        case Status.Found => IO.pure(Left(page.headers.get[Location].map(_.uri.renderString).getOrElse("")))
        case _ =>
          val csrf = page.cookies.find(_.name == "skea_oauth_csrf").map(_.content).getOrElse("")
          val form =
            UrlForm(Uri.unsafeFromString(s"/?$q").query.params.toSeq ++ Seq("csrf" -> csrf, "decision" -> "allow")*)
          routes.run(Request[IO](Method.POST, uri"/oauth/authorize").withEntity(form)
            .putHeaders(Cookie(RequestCookie(ck.name, ck.content), RequestCookie("skea_oauth_csrf", csrf)))).flatMap {
            r =>
              val code = r.headers.get[Location].flatMap(_.uri.params.get("code")).getOrElse("")
              routes.run(Request[IO](Method.POST, uri"/oauth/token").withEntity(UrlForm(
                "grant_type" -> "authorization_code",
                "code" -> code,
                "redirect_uri" -> redirect,
                "code_verifier" -> verifier
              )).putHeaders(Authorization(BasicCredentials(clientId, secret)))).flatMap(_.as[Json]).flatMap { j =>
                val key = j.hcursor.get[String]("access_token").getOrElse("")
                users.findKey(Crypto.sha256(key), Instant.now()).map(k => Right(k.map(_._4).getOrElse(Nil).sorted))
              }
          }
      }
    } yield res
  }

  test("write を許したクライアントは read write の API キー、許していないクライアントは write を求めると invalid_scope") {
    assume(cfg.isDefined, "TEST_DATABASE_URL がない")
    for {
      rw <-
        grant("skakapo", "skakapo-secret-0123456789", "http://localhost:5199/api/connections/callback", "read write")
      _ = assertEquals(rw, Right(List("read", "write")))
      ro <- grant("skiwi", "skiwi-secret-0123456789", "http://localhost:8080/cb", "read")
      _ = assertEquals(ro, Right(List("read")))
      ng <- grant("skiwi", "skiwi-secret-0123456789", "http://localhost:8080/cb", "read write")
      _ = assert(ng.left.exists(_.contains("error=invalid_scope")), ng)
    } yield ()
  }
}
