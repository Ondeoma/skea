package skea.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Decoder
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Authorization
import skea.auth.{ApiKeys, Crypto}
import skea.core.*
import skea.db.*
import skea.shared.*

import java.time.{Instant, LocalDate, ZoneId}
import java.util.UUID
import scala.concurrent.duration.*

final case class ApiConfig(
    adminGroups: Set[String],
    cookieSecure: Boolean,
    publicUrl: String,
    sessionTtl: FiniteDuration = 14.days,
    cookieName: String = "skea_session",
    devLogin: Boolean = false,
    maxUploadBytes: Long = 100L * 1024 * 1024,
    timezone: String = "Asia/Tokyo",
    webhookAllowHttp: Boolean = false,
    version: String = "0.1.0",
    oauthClients: List[OAuthClient] = Nil,
    linkServices: List[LinkService] = Nil
)

/**
 * OAuth で連携する外部サービス（skiwi・skawau: リンクに名前を出す、stui: 通知を送る）。相手に skea をクライアントとして登録しておく。
 * url は利用者のブラウザから見た URL（許可画面・貼られるリンク）、internalUrl はサーバーから呼ぶときの URL（省略時は url）。
 */
final case class LinkService(provider: String, url: String, clientId: String, clientSecret: String, internalUrl: Option[String] = None):
  def server: String = internalUrl.getOrElse(url)

/** 認可を求める外部のサービス（OAUTH_CLIENTS で登録する）。redirectUri は完全一致で照合する。 */
final case class OAuthClient(id: String, secret: String, redirectUri: String, name: String)

/** 処理の失敗（HTTP の応答に変換する）。 */
enum Fail:
  case NotFound(msg: String)
  case Forbidden(msg: String)
  case Invalid(msg: String)
  case Conflict(msg: String)
  case Unauthorized(msg: String)

  def message: String = this match
    case NotFound(m)     => m
    case Forbidden(m)    => m
    case Invalid(m)      => m
    case Conflict(m)     => m
    case Unauthorized(m) => m

object Fail:
  def from(e: StoreError): Fail = e match
    case StoreError.NotFound(w) => Fail.NotFound(s"$w が見つかりません")
    case StoreError.Invalid(m)  => Fail.Invalid(m)
    case StoreError.Conflict(m) => Fail.Conflict(m)

type Res[A] = IO[Either[Fail, A]]

extension [A](io: IO[Either[StoreError, A]]) def orFail: Res[A] = io.map(_.left.map(Fail.from))

/** 操作する人と、自動化の連鎖の深さ（ルールが別のルールを起動する深さ）。 */
final case class Op(s: AuthSession, depth: Int = 0):
  def userId: UUID = s.user.id
  def via: String  = s.viaString
  def today: LocalDate = LocalDate.now(Op.zone(s.user.timezone))

object Op:
  def zone(tz: String): ZoneId = scala.util.Try(ZoneId.of(tz)).getOrElse(ZoneId.of("Asia/Tokyo"))

/** 認証: Cookie のセッション、または `Authorization: Bearer <APIキー>`。 */
final class SessionAuth(users: UserStore, cfg: ApiConfig):

  private def isAdmin(u: User, gs: List[Group]) = u.isAdmin || gs.exists(g => cfg.adminGroups.contains(g.name))

  def fromRequest(req: Request[IO]): IO[Option[AuthSession]] =
    bearer(req) match
      case Some(token) =>
        users.findKey(Crypto.sha256(token), Instant.now()).map(_.map { (u, gs, kid, scopes, projects) =>
          val mcp = req.pathInfo.renderString.startsWith("/mcp") || req.headers.get(org.typelevel.ci.CIString("X-Skea-Mcp")).isDefined
          AuthSession(u, gs, isAdmin(u, gs), AuthVia.ApiKey(kid, scopes.flatMap(Scope.parse).toSet, projects.map(_.toSet), mcp))
        })
      case None =>
        req.cookies.find(_.name == cfg.cookieName).map(_.content) match
          case None        => IO.pure(None)
          case Some(token) => users.findSession(Crypto.sha256(token), Instant.now()).map(_.map((u, gs) => AuthSession(u, gs, isAdmin(u, gs))))

  def publicUrl: String = cfg.publicUrl
  def devLogin: Boolean = cfg.devLogin

  /** 別のユーザーとしての権限確認用（通知の宛先・自動化の実行ユーザー）。無効なユーザーは None。 */
  def sessionFor(userId: UUID, via: AuthVia = AuthVia.Session): IO[Option[AuthSession]] =
    users.withGroups(userId).map(_.map((u, gs) => AuthSession(u, gs, isAdmin(u, gs), via)))

  private def bearer(req: Request[IO]): Option[String] =
    req.headers.get[Authorization].collect { case Authorization(Credentials.Token(AuthScheme.Bearer, t)) => t }.filter(ApiKeys.looksLike)

  def issue(user: User, idToken: Option[String] = None): IO[ResponseCookie] =
    val token = Crypto.randomToken()
    users.createSession(Crypto.sha256(token), user.id, Instant.now().plusSeconds(cfg.sessionTtl.toSeconds), idToken).as(cookie(token, cfg.sessionTtl.toSeconds))

  /** セッションを消す. IdP からもログアウトするための id_token（OIDC でログインしていたとき）と、Cookie を消す指示を返す. */
  def revoke(req: Request[IO]): IO[(Option[String], ResponseCookie)] =
    req.cookies.find(_.name == cfg.cookieName) match
      case Some(c) => users.deleteSession(Crypto.sha256(c.content)).map(_ -> cookie("", 0))
      case None    => IO.pure(None -> cookie("", 0))

  private def cookie(value: String, maxAge: Long): ResponseCookie =
    ResponseCookie(cfg.cookieName, value, maxAge = Some(maxAge), path = Some("/"), httpOnly = true, secure = cfg.cookieSecure,
      sameSite = Some(SameSite.Lax))

/** *Routes に共通の補助（エラー応答・認証・リクエスト本文・権限の確認）。 */
/** 受け取ったファイル（一時ファイル）。 */
final case class Upload(name: String, contentType: String, path: fs2.io.file.Path, size: Long)

trait RouteSupport extends Http4sDsl[IO]:
  protected def sessions: SessionAuth
  protected def access: Access2

  protected def err(status: Status, msg: String): IO[Response[IO]] =
    Response[IO](status).withEntity(ErrorDto(msg)).pure[IO]

  protected def fail(f: Fail): IO[Response[IO]] = f match
    case Fail.NotFound(m)     => err(Status.NotFound, m)
    case Fail.Forbidden(m)    => err(Status.Forbidden, m)
    case Fail.Invalid(m)      => err(Status.BadRequest, m)
    case Fail.Conflict(m)     => err(Status.Conflict, m)
    case Fail.Unauthorized(m) => err(Status.Unauthorized, m)

  /** 結果が Right なら f、Left なら失敗の応答。 */
  protected def respond[A](r: Res[A])(f: A => IO[Response[IO]]): IO[Response[IO]] = r.flatMap(_.fold(fail, f))

  protected def okJson[A: io.circe.Encoder](r: Res[A]): IO[Response[IO]] = respond(r)(a => Ok(a))

  /** POST だが読み取りだけの API（本文で条件を送るもの）。 */
  private val readOnlyPosts = Set("/api/tasks/lookup")
  private def readOnlyPost(req: Request[IO]): Boolean = req.method == Method.POST && readOnlyPosts.contains(req.pathInfo.renderString)

  private val safeMethods = Set(Method.GET, Method.HEAD, Method.OPTIONS)

  /** Cookie のセッションでの更新系の要求が、別のサイトから送られたものでないか（CSRF 対策）。
    * ブラウザが付ける Origin（なければ Referer）が PUBLIC_URL と同じ origin であることを確かめる。開発中（DEV_LOGIN）は localhost も許す。 */
  private def sameOrigin(req: Request[IO]): Boolean =
    def origin(u: String): Option[String] =
      Uri.fromString(u).toOption.flatMap(x => (x.scheme, x.authority).mapN((sc, a) => s"${sc.value.toString.toLowerCase}://${a.host.value.toLowerCase}${a.port.fold("")(":" + _)}"))
    val mine = origin(sessions.publicUrl)
    def ok(o: String): Boolean = origin(o).exists(x => mine.contains(x) ||
      (sessions.devLogin && Uri.fromString(o).toOption.flatMap(_.host).exists(h => Set("localhost", "127.0.0.1").contains(h.value))))
    req.headers.get(org.typelevel.ci.CIString("Origin")).map(_.head.value).filter(_ != "null") match
      case Some(o) => ok(o)
      case None => req.headers.get(org.typelevel.ci.CIString("Referer")).map(_.head.value) match
          case Some(r) => ok(r)
          // Origin も Referer もない: ブラウザ以外（curl など）。ブラウザが別サイトからと示していれば拒否する
          case None => !req.headers.get(org.typelevel.ci.CIString("Sec-Fetch-Site")).exists(h => h.head.value == "cross-site" || h.head.value == "same-site")

  /** ログインしていなければ 401。更新系の要求を read スコープのキーで送ったら 403。 */
  protected def authed(req: Request[IO])(f: AuthSession => IO[Response[IO]]): IO[Response[IO]] =
    sessions.fromRequest(req).flatMap {
      case None => err(Status.Unauthorized, "ログインしてください")
      case Some(s) =>
        // 操作したユーザーを、トレース（スパンの属性）とこの中のログに載せる
        skea.observe.Telemetry.withUser(s.user.id.toString, s.user.name) {
          if s.isSession && !safeMethods.contains(req.method) && !sameOrigin(req) then
            err(Status.Forbidden, "別のサイトからの要求は受け付けません")
          else if !s.canWrite && !safeMethods.contains(req.method) && !readOnlyPost(req) then
            err(Status.Forbidden, "このAPIキーは参照専用です（write スコープが必要です）")
          else f(s)
        }
    }

  /** システム管理者のみ（APIキーは admin スコープが必要）。 */
  protected def admin(req: Request[IO])(f: AuthSession => IO[Response[IO]]): IO[Response[IO]] =
    authed(req)(s => if s.canAdmin then f(s) else err(Status.Forbidden, "管理者だけが使えます"))

  /** Cookie のセッションのみ（APIキーの発行など）。 */
  protected def sessionOnly(req: Request[IO])(f: AuthSession => IO[Response[IO]]): IO[Response[IO]] =
    authed(req)(s => if s.isSession then f(s) else err(Status.Forbidden, "この操作は画面からのみ行えます"))

  protected def body[A: Decoder](req: Request[IO])(f: A => IO[Response[IO]]): IO[Response[IO]] =
    req.as[A].attempt.flatMap {
      case Left(e)  => err(Status.BadRequest, s"リクエストの内容が正しくありません: ${e.getMessage.take(200)}")
      case Right(b) => f(b)
    }

  /** プロジェクトの権限を確認してから処理する。 */
  protected def withProject(req: Request[IO], pid: UUID, need: Role)(f: (AuthSession, Role) => IO[Response[IO]]): IO[Response[IO]] =
    authed(req)(s => respond(access.project(s, pid, need))(r => f(s, r)))

  /** タスクの権限を確認してから処理する。 */
  protected def withTask(req: Request[IO], id: String, need: Role)(f: (AuthSession, TaskCore, Role) => IO[Response[IO]]): IO[Response[IO]] =
    authed(req)(s => respond(access.task(s, id, need))((t, r) => f(s, t, r)))

  /** multipart/form-data の file を一時ファイルに受け取る（上限を超えたら 413）。 */
  protected def withUpload(req: Request[IO], maxBytes: Long)(f: Upload => IO[Response[IO]]): IO[Response[IO]] =
    req.as[org.http4s.multipart.Multipart[IO]].attempt.flatMap {
      case Left(_) => err(Status.BadRequest, "multipart/form-data で file を送ってください")
      case Right(mp) =>
        mp.parts.find(_.name.contains("file")) match
          case None => err(Status.BadRequest, "file がありません")
          case Some(part) =>
            val name = part.filename.map(_.trim).filter(_.nonEmpty).getOrElse("file").replaceAll("[\\\\/\\r\\n]", "_").take(200)
            val ct   = part.contentType.map(c => s"${c.mediaType.mainType}/${c.mediaType.subType}").getOrElse("application/octet-stream")
            fs2.io.file.Files[IO].tempFile.use { tmp =>
              part.body.take(maxBytes + 1).through(fs2.io.file.Files[IO].writeAll(tmp)).compile.drain *>
                fs2.io.file.Files[IO].size(tmp).flatMap { size =>
                  if size > maxBytes then err(Status.PayloadTooLarge, s"ファイルが大きすぎます（上限 ${math.max(1, maxBytes / 1024 / 1024)}MB）")
                  else f(Upload(name, ct, tmp, size))
                }
            }
    }

  protected object UuidVar:
    def unapply(s: String): Option[UUID] = uuidOpt(s)

  protected def param(req: Request[IO], k: String): Option[String] = req.params.get(k).map(_.trim).filter(_.nonEmpty)

  protected def intParam(req: Request[IO], k: String, default: Int, max: Int): Int =
    param(req, k).flatMap(_.toIntOption).map(v => math.max(1, math.min(v, max))).getOrElse(default)

/** 権限の確認（ルートとサービスで共通）。見えないものは 404、権限が足りなければ 403。 */
final class Access2(projects: ProjectStore, tasks: TaskStore):

  def project(s: AuthSession, pid: UUID, need: Role): Res[Role] =
    projects.role(pid, s).map {
      case None                    => Left(Fail.NotFound("プロジェクトが見つかりません"))
      // ヘルプデスクの"ユーザ"には、エージェント向けのものはあることも知らせない
      case Some(r) if !r.isAgent && need.isAgent => Left(Fail.NotFound("プロジェクトが見つかりません"))
      case Some(r) if r < need     => Left(Fail.Forbidden(if need == Role.Admin then "プロジェクトの管理権限が必要です" else "編集権限が必要です"))
      case Some(r)                 => Right(r)
    }

  /** id は UUID またはタスクID（SKEA-12）。 */
  def task(s: AuthSession, id: String, need: Role): Res[(TaskCore, Role)] =
    (uuidOpt(id) match
      case Some(u) => tasks.core(u)
      case None    => tasks.coreByKey(id)
    ).flatMap {
      case None => IO.pure(Left(Fail.NotFound("タスクが見つかりません")))
      case Some(t) => project(s, t.projectId, need).map {
        case Left(Fail.NotFound(_)) => Left(Fail.NotFound("タスクが見つかりません"))
        case other                  => other.map(t -> _)
      }
    }

/** 日付の入力（yyyy-MM-dd）。 */
object Dates:
  def parse(s: Option[String], what: String): Either[Fail, Option[LocalDate]] =
    s.map(_.trim).filter(_.nonEmpty) match
      case None    => Right(None)
      case Some(v) => scala.util.Try(LocalDate.parse(v)).toOption.toRight(Fail.Invalid(s"$what は yyyy-MM-dd で指定してください")).map(Some(_))
