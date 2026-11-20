package skea.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import sbirds.auth.Crypto
import sbirds.oauth.{OAuthCode, OAuthProvider}
import skea.auth.ApiKeys
import skea.core.*
import skea.db.*
import skea.shared.*

import java.util.UUID

/**
 * 外部のサービス（skiwi など）への認可（OAuth 2.0 の認可コード + PKCE。流れは sbirds.oauth.OAuthProvider）と、タスクの一括参照。
 * 発行するトークンは、そのユーザーの API キー（「<クライアント名> 連携」。スコープは求められたもの（read・クライアントに許していれば write）。API キーの画面から失効できる）。
 */
final class OAuthRoutes(oauth: OAuthStore,
                        tasks: TaskStore,
                        projects: ProjectStore,
                        activity: ActivityStore,
                        val sessions: SessionAuth,
                        val access: Access2,
                        cfg: ApiConfig) extends RouteSupport {

  private val host = new OAuthProvider.Host[AuthSession] {
    val settings = OAuthProvider.Settings("skea", cfg.cookieSecure)
    val codes = oauth

    def client(id: String): IO[Option[OAuthClient]] = IO.pure(cfg.oauthClients.find(_.id == id))

    // 許可するのは画面にログインしている本人だけ（API キーでは許可させない. 範囲を絞ったキーから、広い権限のキーを作らせない）
    def browserSession(req: Request[IO]): IO[Option[AuthSession]] = sessions.fromRequest(req).map(_.filter(_.isSession))

    def userId(s: AuthSession): UUID = s.user.id

    override def scopes(c: OAuthClient, requested: Option[String]): Option[List[String]] =
      OAuthProvider.requestedScopes(c.scopes, requested, List(ApiScope.Read))

    def consent(req: Request[IO], c: OAuthClient, s: AuthSession, scopes: List[String]): IO[OAuthProvider.Consent] = {
      val name = Html.escape(c.name)
      val write = scopes.contains(ApiScope.Write)
      IO.pure(OAuthProvider.Consent(
        s"$name との連携",
        s"""<h1>$name との連携</h1>
           |<p><b>$name</b> が、あなた（${Html.escape(s.user.name)}）の権限で skea のタスクを${
            if write then "読み書き" else "読み取り"
          }します。</p>
           |<ul class="muted">${
            if write then "<li>タスクの作成・変更・コメントを、あなたの名前で行います</li>" else "<li>読み取りだけです（作成・変更・削除はしません）</li>"
          }<li>あなたが見られるプロジェクトのタスクだけが対象です</li>
           |<li>個人設定の「APIキー」から、いつでも連携をやめられます</li></ul>""".stripMargin
      ))
    }

    override def authorized(req: Request[IO], s: AuthSession, c: OAuthClient, scopes: List[String]): IO[Unit] =
      activity.audit(Some(s.user.id), "web", "oauth.authorize", Some(c.id), Json.obj())

    def issue(req: Request[IO], c: OAuthClient, oc: OAuthCode): IO[OAuthProvider.Token] = {
      val (key, prefix) = ApiKeys.generate()
      oauth.replaceKey(
        oc.userId,
        c.id,
        s"${c.name} 連携",
        prefix,
        Crypto.sha256(key),
        oc.scope.split(' ').toList.filter(x => x == ApiScope.Read || x == ApiScope.Write).distinct.sorted
      ) *>
        activity.audit(Some(oc.userId), "oauth", "api_key.create", Some(c.id), Json.obj("client" -> c.id.asJson))
          .as(OAuthProvider.Token(key, oc.scope))
    }

    def revoke(req: Request[IO], c: OAuthClient, token: String): IO[Unit] =
      oauth.revokeKey(Crypto.sha256(token), c.id)
  }

  val routes: HttpRoutes[IO] = new OAuthProvider(host).routes <+> HttpRoutes.of[IO] {
    case req @ POST -> Root / "api" / "tasks" / "lookup" => postTasksLookup(req)
  }

  private def postTasksLookup(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s =>
      body[Json](req) { j =>
        val keys = j.hcursor.get[List[String]]("keys").getOrElse(Nil).map(_.trim.toUpperCase).filter(TaskKeys.parse(
          _
        ).isDefined).distinct
        if keys.size > 100 then err(ErrorCode.OAuthKeysTooMany)
        else
          (projects.visible(s), tasks.lookup(keys)).tupled.flatMap {
            (vis, rows) =>
              {
                val allowed = s.via match {
                  case AuthVia.ApiKey(_, _, Some(ps), _) => (p: UUID) => vis.contains(p) && ps.contains(p)
                  case _ => (p: UUID) => vis.contains(p)
                }
                Ok(Json.obj("tasks" -> rows.filter(r => allowed(r._1)).map {
                  (_, key, title, name, cat, color) =>
                    Json.obj(
                      "key" -> key.asJson,
                      "title" -> title.asJson,
                      "status" -> Json.obj("name" -> name.asJson, "category" -> cat.asJson, "color" -> color.asJson)
                    )
                }.asJson))
              }
          }
      }
    )
}
