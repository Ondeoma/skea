package skea.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.{Decoder, Json}
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.headers.{Authorization, Location}
import skea.auth.{Crypto, SecretBox}
import skea.db.*
import skea.shared.*

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

/**
 * 外部サービスのリンクに名前を出す連携（docs/skiwi-integration.md・docs/skawau-integration.md）。
 * 各ユーザーが OAuth 2.0（認可コード + PKCE）で相手のサービスを許可し、skea は本人の権限で、貼られたリンク先の名前
 * （skiwi のページのタイトル・skawau のファイル名）を読む。トークンは APP_SECRET の鍵で暗号化して保存する。
 */
final class LinkServiceRoutes(store: IntegrationTokenStore, http: Client[IO], box: SecretBox, cache: LinkServiceRoutes.Cache,
    val sessions: SessionAuth, val access: Access2, cfg: ApiConfig)
    extends RouteSupport:

  import LinkServiceRoutes.*

  private val CacheMillis = 60.seconds.toMillis

  private def callback(p: String): String = s"${cfg.publicUrl}/api/integrations/$p/callback"

  private def basic(c: LinkService) = Authorization(BasicCredentials(c.clientId, c.clientSecret))

  private def redirect(to: String): IO[Response[IO]] = IO.pure(Response[IO](Status.Found).putHeaders(Location(Uri.unsafeFromString(to))))

  /** 個人設定へ戻る（結果を integration=…&result=… に付ける）。 */
  private def done(p: String, returnTo: String, result: String): IO[Response[IO]] =
    // 同じサイトのパスだけ。開発中（DEV_LOGIN）は画面の開発サーバー（http://localhost:…）にも戻せる
    val devOrigin = cfg.devLogin && (returnTo.startsWith("http://localhost:") || returnTo.startsWith("http://127.0.0.1:"))
    val back = Option(returnTo).filter(p => (p.startsWith("/") && !p.startsWith("//")) || devOrigin).getOrElse("/settings")
    redirect(back + (if back.contains("?") then "&" else "?") + s"integration=$p&result=$result")

  private def service(p: String): Option[LinkService] = cfg.linkServices.find(_.provider == p)

  private def withService(p: String)(f: LinkService => IO[Response[IO]]): IO[Response[IO]] =
    service(p).fold(err(Status.NotFound, s"$p との連携は設定されていません"))(f)

  private def tokenOf(uid: UUID, p: String): IO[Option[String]] = store.token(uid, p).map(_.flatMap(box.decrypt))

  private def forget(uid: UUID, p: String): IO[Unit] = cache.update(_.filterNot(k => k._1._1 == p && k._1._2 == uid))

  /** 本人のトークンでリンク先の名前をまとめて読む（60秒キャッシュ。見られないものも「なし」として覚える）。トークンが失効していたら None。 */
  private def lookup(c: LinkService, uid: UUID, token: String, ids: List[UUID]): IO[Option[List[LinkTitleDto]]] =
    val kind = kinds(c.provider)
    for
      now   <- IO.realTime.map(_.toMillis)
      known <- cache.get.map(m => ids.flatMap(id => m.get((c.provider, uid, id)).filter(_._1 > now).map(id -> _._2)).toMap)
      miss   = ids.filterNot(known.contains)
      fetched <- if miss.isEmpty then IO.pure(Some(Nil))
                 else
                   val req = Request[IO](Method.POST, Uri.unsafeFromString(c.url + kind.lookupPath))
                     .withEntity(Json.obj("ids" -> miss.asJson))
                     .putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, token)), "Accept" -> "application/json")
                   http.run(req).use { r =>
                     if r.status == Status.Unauthorized then IO.pure(None)
                     else if !r.status.isSuccess then IO.raiseError(new RuntimeException(s"${c.provider} が ${r.status.code} を返しました"))
                     else r.as[Json].map(j => Some(j.hcursor.downField(kind.listKey).as[List[Json]].getOrElse(Nil).flatMap(kind.parse)))
                   }.timeout(10.seconds)
      res <- fetched match
        case None => IO.pure(None)
        case Some(ts) =>
          cache.update(m => (m ++ miss.map(id => (c.provider, uid, id) -> (now + CacheMillis, ts.find(_.id == id)))).filter(_._2._1 > now)).as(
            Some(ids.flatMap(id => known.get(id).flatten.orElse(ts.find(_.id == id)))))
    yield res

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    /** 設定されたサービスと、自分が連携済みかどうか。 */
    case req @ GET -> Root / "api" / "integrations" / "links" =>
      authed(req)(s => cfg.linkServices.traverse(c => store.token(s.user.id, c.provider).map(t =>
        LinkServiceDto(c.provider, kinds(c.provider).name, c.url, t.isDefined))).flatMap(Ok(_)))

    /** 連携を始める（相手のサービスの許可画面へ）。 */
    case req @ GET -> Root / "api" / "integrations" / p / "connect" if kinds.contains(p) =>
      sessionOnly(req)(s => withService(p) { c =>
        val state    = Crypto.randomToken(24)
        val verifier = Crypto.randomToken(48)
        store.saveState(state, s.user.id, p, verifier, param(req, "returnTo").getOrElse("/settings"), Instant.now().plusSeconds(600)) *>
          redirect(Uri.unsafeFromString(s"${c.url}/oauth/authorize").withQueryParams(Map(
            "response_type" -> "code", "client_id" -> c.clientId, "redirect_uri" -> callback(p), "scope" -> "read", "state" -> state,
            "code_challenge" -> Crypto.pkceChallenge(verifier), "code_challenge_method" -> "S256")).renderString)
      })

    /** 許可画面から戻ったとき: コードをトークンに替えて保存する。 */
    case req @ GET -> Root / "api" / "integrations" / p / "callback" if kinds.contains(p) =>
      sessionOnly(req)(s => withService(p) { c =>
        param(req, "state").fold(IO.pure(None))(st => store.takeState(st, p)).flatMap {
          case Some((uid, _, back)) if uid != s.user.id => done(p, back, "error")
          case None => done(p, "/settings", "error")
          case Some((_, _, back)) if param(req, "error").isDefined => done(p, back, if param(req, "error").contains("access_denied") then "denied" else "error")
          case Some((_, verifier, back)) =>
            param(req, "code") match
              case None => done(p, back, "error")
              case Some(code) =>
                val form = UrlForm("grant_type" -> "authorization_code", "code" -> code, "redirect_uri" -> callback(p), "code_verifier" -> verifier)
                http.run(Request[IO](Method.POST, Uri.unsafeFromString(s"${c.url}/oauth/token")).withEntity(form).putHeaders(basic(c)))
                  .use(r => if r.status.isSuccess then r.as[Json].map(_.hcursor.get[String]("access_token").toOption) else IO.pure(None))
                  .timeout(15.seconds).handleError(_ => None).flatMap {
                    case Some(token) => store.saveToken(s.user.id, p, box.encrypt(token)) *> forget(s.user.id, p) *> done(p, back, "connected")
                    case None        => done(p, back, "error")
                  }
        }
      })

    /** 連携をやめる（相手のトークンも失効させる）。 */
    case req @ DELETE -> Root / "api" / "integrations" / p / "connection" if kinds.contains(p) =>
      sessionOnly(req)(s => withService(p) { c =>
        tokenOf(s.user.id, p).flatMap(_.traverse_ { t =>
          http.run(Request[IO](Method.POST, Uri.unsafeFromString(s"${c.url}/oauth/revoke")).withEntity(UrlForm("token" -> t)).putHeaders(basic(c)))
            .use(_ => IO.unit).timeout(10.seconds).handleError(_ => ())
        }) *> store.deleteToken(s.user.id, p) *> forget(s.user.id, p) *> NoContent()
      })

    /** 貼られたリンク先の名前（本人の権限で見られるものだけ）。 */
    case req @ POST -> Root / "api" / "integrations" / p / "titles" if kinds.contains(p) =>
      authed(req)(s => body[LinkTitlesReq](req) { r =>
        val ids = r.ids.distinct
        if ids.size > 100 then err(Status.BadRequest, "ids は100件までです")
        else service(p) match
          case None => Ok(LinkTitlesDto(Nil, connected = false))
          case Some(c) =>
            tokenOf(s.user.id, p).flatMap {
              case None => Ok(LinkTitlesDto(Nil, connected = false))
              case Some(t) =>
                lookup(c, s.user.id, t, ids).attempt.flatMap {
                  case Right(Some(ts)) => Ok(LinkTitlesDto(ts, connected = true))
                  // 相手でトークンが失効していたら連携を外す
                  case Right(None) => store.deleteToken(s.user.id, p) *> Ok(LinkTitlesDto(Nil, connected = false))
                  case Left(e)     => err(Status.BadGateway, s"$p に問い合わせできませんでした: ${e.getMessage.take(200)}")
                }
            }
      })
  }

object LinkServiceRoutes:

  /** (サービス, ユーザー, ID) → (期限, 名前) */
  type Cache = Ref[IO, Map[(String, UUID, UUID), (Long, Option[LinkTitleDto])]]

  def newCache: IO[Cache] = Ref.of(Map.empty)

  /** サービスごとの違い: 表示名、問い合わせの API、応答の配列の名前、1件の読み方。 */
  final case class Kind(name: String, lookupPath: String, listKey: String, parse: Json => Option[LinkTitleDto])

  private def field[A: Decoder](j: Json, k: String): Option[A] = j.hcursor.get[A](k).toOption

  /** skiwi: {"pages":[{"id","workspaceId","title","icon"}]} */
  private def skiwiPage(j: Json): Option[LinkTitleDto] =
    (field[UUID](j, "id"), field[String](j, "title")).mapN((id, t) => LinkTitleDto(id, t, field[Option[String]](j, "icon").flatten.orElse(Some("📄"))))

  /** skawau: {"files":[{"id","name","folder"}]} */
  private def skawauFile(j: Json): Option[LinkTitleDto] =
    (field[UUID](j, "id"), field[String](j, "name")).mapN((id, n) => LinkTitleDto(id, n, Some(fileIcon(n, field[Boolean](j, "folder").contains(true)))))

  /** ファイル名の拡張子から選ぶアイコン。 */
  def fileIcon(name: String, folder: Boolean): String =
    if folder then "📁"
    else
      val ext = name.split('.').toList match
        case _ :: rest if rest.nonEmpty => rest.last.toLowerCase
        case _                          => ""
      ext match
        case "png" | "jpg" | "jpeg" | "gif" | "webp" | "svg" | "bmp" | "heic" => "🖼️"
        case "pdf"                                                          => "📕"
        case "doc" | "docx" | "odt" | "rtf"                                  => "📝"
        case "xls" | "xlsx" | "ods" | "csv"                                  => "📊"
        case "ppt" | "pptx" | "odp"                                          => "📽️"
        case "mp4" | "mov" | "webm" | "mkv" | "avi"                          => "🎬"
        case "mp3" | "wav" | "m4a" | "ogg" | "flac"                          => "🎵"
        case "zip" | "gz" | "tgz" | "7z" | "rar" | "tar"                     => "🗜️"
        case _                                                              => "📄"

  val kinds: Map[String, Kind] = Map(
    "skiwi"  -> Kind("skiwi", "/api/pages/lookup", "pages", skiwiPage),
    "skawau" -> Kind("skawau", "/api/files/lookup", "files", skawauFile)
  )

  /** 環境変数を読む順。 */
  val providers: List[String] = List("skiwi", "skawau")
