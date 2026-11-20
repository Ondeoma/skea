package skea.api

import cats.effect.*
import org.http4s.*
import sbirds.auth.Crypto
import sbirds.http.{HttpParts, SessionCookie}
import skea.auth.ApiKeys
import skea.core.*
import skea.db.*

import java.time.Instant
import java.util.UUID

/** 認証: Cookie のセッション、または `Authorization: Bearer <APIキー>`。 */
final class SessionAuth(users: UserStore, cfg: ApiConfig) {

  private val cookie = new SessionCookie(cfg.cookieName, cfg.cookieSecure, cfg.sessionTtl)

  private def isAdmin(u: User, gs: List[Group]) = u.isAdmin || gs.exists(g => cfg.adminGroups.contains(g.name))

  def fromRequest(req: Request[IO]): IO[Option[AuthSession]] =
    bearer(req) match {
      case Some(token) =>
        users.findKey(Crypto.sha256(token), Instant.now()).map(_.map {
          (u, gs, kid, scopes, projects) =>
            {
              val mcp = req.pathInfo.renderString.startsWith("/mcp") ||
                req.headers.get(org.typelevel.ci.CIString("X-Skea-Mcp")).isDefined
              AuthSession(
                u,
                gs,
                isAdmin(u, gs),
                AuthVia.ApiKey(kid, scopes.flatMap(Scope.parse).toSet, projects.map(_.toSet), mcp)
              )
            }
        })
      case None =>
        cookie.hash(req) match {
          case None => IO.pure(None)
          case Some(hash) =>
            users.findSession(hash, Instant.now()).map(_.map((u, gs) => AuthSession(u, gs, isAdmin(u, gs))))
        }
    }

  def publicUrl: String = cfg.publicUrl
  def devLogin: Boolean = cfg.devLogin

  /** 別のユーザーとしての権限確認用（通知の宛先・自動化の実行ユーザー）。無効なユーザーは None。 */
  def sessionFor(userId: UUID, via: AuthVia = AuthVia.Session): IO[Option[AuthSession]] =
    users.withGroups(userId).map(_.map((u, gs) => AuthSession(u, gs, isAdmin(u, gs), via)))

  private def bearer(req: Request[IO]): Option[String] = HttpParts.bearer(req).filter(ApiKeys.looksLike)

  def issue(user: User, idToken: Option[String] = None): IO[ResponseCookie] =
    cookie.issue((hash, expires) => users.createSession(hash, user.id, expires, idToken))

  /** sbirds のアプリの表示名（ログインしたときの ID トークンにある. API キーや開発用のログインでは空）. */
  def appNames(req: Request[IO]): IO[Map[String, String]] = cookie.appNames(req)(users.sessionIdToken)

  /** セッションを消す. IdP からもログアウトするための id_token（OIDC でログインしていたとき）と、Cookie を消す指示を返す. */
  def revoke(req: Request[IO]): IO[(Option[String], ResponseCookie)] = cookie.revoke(req)(users.deleteSession)
}
