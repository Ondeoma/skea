package skea.api

import sbirds.links.IntegrationTokenStore

import skea.core.ErrorCode

import cats.effect.*
import cats.syntax.all.*
import io.circe.{Decoder, Json}
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.headers.{Authorization, Location}
import sbirds.auth.Crypto
import skea.auth.SecretBox
import skea.shared.*

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

/**
 * 外部サービスとの OAuth の連携（docs/skiwi-integration.md・docs/skawau-integration.md・docs/stui-integration.md）。
 * 各ユーザーが OAuth 2.0（認可コード + PKCE）で相手のサービスを許可し、skea は本人の権限で、貼られたリンク先の名前
 * （skiwi のページのタイトル・skawau のファイル名）を読む。stui には通知を送る（ChatNotifier）。トークンは APP_SECRET の鍵で暗号化して保存する。
 */
final class LinkServiceRoutes(store: IntegrationTokenStore,
                              http: Client[IO],
                              box: SecretBox,
                              cache: LinkServiceRoutes.Cache,
                              val sessions: SessionAuth,
                              val access: Access2,
                              cfg: ApiConfig)
  extends RouteSupport {
  import LinkServiceRoutes.*

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

    /** 設定されたサービスと、自分が連携済みかどうか。 */
    case req @ GET -> Root / "api" / "integrations" / "links" => getIntegrationsLinks(req)
    case req @ GET -> Root / "api" / "integrations" / p / "connect" if kinds.contains(p) =>
      getIntegrationConnect(req, p)
    case req @ GET -> Root / "api" / "integrations" / p / "callback" if kinds.contains(p) =>
      getIntegrationCallback(req, p)
    case req @ DELETE -> Root / "api" / "integrations" / p / "connection" if kinds.contains(p) =>
      deleteIntegrationConnection(req, p)
    case req @ POST -> Root / "api" / "integrations" / p / "titles" if kinds.contains(p) =>
      postIntegrationTitles(req, p)
  }

  private val CacheMillis = 60.seconds.toMillis

  private def callback(p: String): String = s"${cfg.publicUrl}/api/integrations/$p/callback"

  private def basic(c: LinkService) = Authorization(BasicCredentials(c.clientId, c.clientSecret))

  private def redirect(to: String): IO[Response[IO]] =
    IO.pure(Response[IO](Status.Found).putHeaders(Location(Uri.unsafeFromString(to))))

  /** 個人設定へ戻る（結果を integration=…&result=… に付ける）。 */
  private def done(p: String, returnTo: String, result: String): IO[Response[IO]] = {
    // 同じサイトのパスだけ。開発中（DEV_LOGIN）は画面の開発サーバー（http://localhost:…）にも戻せる
    val devOrigin = cfg.devLogin && Uri.fromString(returnTo).toOption.exists(u =>
      u.scheme.exists(_.value.toString == "http") &&
        u.authority.exists(a => a.userInfo.isEmpty && Set("localhost", "127.0.0.1").contains(a.host.value))
    )
    val back = Option(returnTo).filter(p =>
      (p.startsWith("/") && !p.startsWith("//") && !p.startsWith("/\\")) || devOrigin
    ).getOrElse("/settings")
    redirect(back + (if back.contains("?") then "&" else "?") + s"integration=$p&result=$result")
  }

  private def service(p: String): Option[LinkService] = cfg.linkServices.find(_.provider == p)

  private def withService(p: String)(f: LinkService => IO[Response[IO]]): IO[Response[IO]] =
    service(p).fold(err(ErrorCode.LinkServiceNotConfigured, s"$p との連携は設定されていません"))(f)

  private def tokenOf(uid: UUID, p: String): IO[Option[String]] = store.token(uid, p).map(_.flatMap(box.decrypt))

  private def forget(uid: UUID, p: String): IO[Unit] = cache.update(_.filterNot(k => k._1._1 == p && k._1._2 == uid))

  /** 本人のトークンでリンク先の名前をまとめて読む（60秒キャッシュ。見られないものも「なし」として覚える）。トークンが失効していたら None。 */
  private def lookup(c: LinkService,
                     kind: Lookup,
                     uid: UUID,
                     token: String,
                     ids: List[UUID]): IO[Option[List[LinkTitleDto]]] =
    for {
      now <- IO.realTime.map(_.toMillis)
      known <-
        cache.get.map(m => ids.flatMap(id => m.get((c.provider, uid, id)).filter(_._1 > now).map(id -> _._2)).toMap)
      miss = ids.filterNot(known.contains)
      fetched <- if miss.isEmpty then IO.pure(Some(Nil))
      else {
        val req = Request[IO](Method.POST, Uri.unsafeFromString(c.server + kind.path))
          .withEntity(Json.obj("ids" -> miss.asJson))
          .putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, token)), "Accept" -> "application/json")
        http.run(req).use { r =>
          if r.status == Status.Unauthorized then IO.pure(None)
          else if !r.status.isSuccess then
            IO.raiseError(new RuntimeException(s"${c.provider} が ${r.status.code} を返しました"))
          else
            r.as[Json].map(j =>
              Some(j.hcursor.downField(kind.listKey).as[List[Json]].getOrElse(Nil).flatMap(kind.parse))
            )
        }.timeout(10.seconds)
      }
      res <- fetched match {
        case None => IO.pure(None)
        case Some(ts) =>
          cache.update(m =>
            (m ++ miss.map(id => (c.provider, uid, id) -> (now + CacheMillis, ts.find(_.id == id)))).filter(_._2._1 >
              now)
          ).as(
            Some(ids.flatMap(id => known.get(id).flatten.orElse(ts.find(_.id == id))))
          )
      }
    } yield res

  private def getIntegrationsLinks(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s =>
      sessions.appNames(req).flatMap(names =>
        cfg.linkServices.traverse(c =>
          store.token(s.user.id, c.provider).map(t =>
            // 名前は sruru に登録した表示名（なければサービスの名前）
            LinkServiceDto(c.provider, names.getOrElse(c.provider, kinds(c.provider).name), c.url, t.isDefined)
          )
        )
      ).flatMap(Ok(_))
    )

  private def getIntegrationConnect(req: Request[IO], p: String): IO[Response[IO]] =
    sessionOnly(req)(s =>
      withService(p) { c =>
        val state = Crypto.randomToken(24)
        val verifier = Crypto.randomToken(48)
        store.saveState(
          state,
          s.user.id,
          p,
          verifier,
          param(req, "returnTo").getOrElse("/settings"),
          Instant.now().plusSeconds(600)
        ) *>
          redirect(Uri.unsafeFromString(s"${c.url}/oauth/authorize").withQueryParams(Map(
            "response_type" -> "code",
            "client_id" -> c.clientId,
            "redirect_uri" -> callback(p),
            "scope" -> kinds(p).scope,
            "state" -> state,
            "code_challenge" -> Crypto.pkceChallenge(verifier),
            "code_challenge_method" -> "S256"
          )).renderString)
      }
    )

  private def getIntegrationCallback(req: Request[IO], p: String): IO[Response[IO]] =
    sessionOnly(req)(s =>
      withService(p) { c =>
        param(req, "state").fold(IO.pure(None))(st => store.takeState(st, p)).flatMap {
          case Some((uid, _, back)) if uid != s.user.id => done(p, back, "error")
          case None => done(p, "/settings", "error")
          case Some((_, _, back)) if param(req, "error").isDefined =>
            done(p, back, if param(req, "error").contains("access_denied") then "denied" else "error")
          case Some((_, verifier, back)) =>
            param(req, "code") match {
              case None => done(p, back, "error")
              case Some(code) =>
                val form = UrlForm(
                  "grant_type" -> "authorization_code",
                  "code" -> code,
                  "redirect_uri" -> callback(p),
                  "code_verifier" -> verifier
                )
                http.run(Request[IO](
                  Method.POST,
                  Uri.unsafeFromString(s"${c.server}/oauth/token")
                ).withEntity(form).putHeaders(basic(c)))
                  .use(r =>
                    if r.status.isSuccess then r.as[Json].map(_.hcursor.get[String]("access_token").toOption)
                    else IO.pure(None)
                  )
                  .timeout(15.seconds).handleError(_ => None).flatMap {
                    case Some(token) => store.saveToken(s.user.id, p, box.encrypt(token)) *> forget(s.user.id, p) *>
                        done(p, back, "connected")
                    case None => done(p, back, "error")
                  }
            }
        }
      }
    )

  private def deleteIntegrationConnection(req: Request[IO], p: String): IO[Response[IO]] =
    sessionOnly(req)(s =>
      withService(p) { c =>
        tokenOf(s.user.id, p).flatMap(_.traverse_ { t =>
          http.run(Request[IO](
            Method.POST,
            Uri.unsafeFromString(s"${c.server}/oauth/revoke")
          ).withEntity(UrlForm("token" -> t)).putHeaders(basic(c)))
            .use(_ => IO.unit).timeout(10.seconds).handleError(_ => ())
        }) *> store.deleteToken(s.user.id, p) *> forget(s.user.id, p) *> NoContent()
      }
    )

  private def postIntegrationTitles(req: Request[IO], p: String): IO[Response[IO]] =
    authed(req)(s =>
      body[LinkTitlesReq](req) { r =>
        val ids = r.ids.distinct
        if ids.size > 100 then err(ErrorCode.LinkTitlesTooMany)
        else
          (service(p), kinds(p).lookup) match {
            case (None, _) => Ok(LinkTitlesDto(Nil, connected = false))
            case (Some(_), None) =>
              store.token(s.user.id, p).flatMap(t => Ok(LinkTitlesDto(Nil, connected = t.isDefined)))
            case (Some(c), Some(lk)) =>
              tokenOf(s.user.id, p).flatMap {
                case None => Ok(LinkTitlesDto(Nil, connected = false))
                case Some(t) =>
                  lookup(c, lk, s.user.id, t, ids).attempt.flatMap {
                    case Right(Some(ts)) => Ok(LinkTitlesDto(ts, connected = true))
                    // 相手でトークンが失効していたら連携を外す
                    case Right(None) => store.deleteToken(s.user.id, p) *> Ok(LinkTitlesDto(Nil, connected = false))
                    case Left(e) =>
                      err(ErrorCode.LinkServiceUnreachable, s"$p に問い合わせできませんでした: ${e.getMessage.take(200)}")
                  }
              }
          }
      }
    )
}

object LinkServiceRoutes {

  /** (サービス, ユーザー, ID) → (期限, 名前) */
  type Cache = Ref[IO, Map[(String, UUID, UUID), (Long, Option[LinkTitleDto])]]

  def newCache: IO[Cache] = Ref.of(Map.empty)

  /** リンク先の名前の問い合わせ: API、応答の配列の名前、1件の読み方。 */
  final case class Lookup(path: String, listKey: String, parse: Json => Option[LinkTitleDto])

  /** サービスごとの違い: 表示名、求めるスコープ、名前の問い合わせ（stui は通知を送るだけなので持たない）。 */
  final case class Kind(name: String, scope: String, lookup: Option[Lookup])

  private def field[A: Decoder](j: Json, k: String): Option[A] = j.hcursor.get[A](k).toOption

  /** skiwi: {"pages":[{"id","workspaceId","title","icon"}]} */
  private def skiwiPage(j: Json): Option[LinkTitleDto] =
    (field[UUID](j, "id"), field[String](j, "title")).mapN((id, t) =>
      LinkTitleDto(id, t, field[Option[String]](j, "icon").flatten.orElse(Some("📄")))
    )

  /** skawau: {"files":[{"id","name","folder"}]} */
  private def skawauFile(j: Json): Option[LinkTitleDto] =
    (field[UUID](j, "id"), field[String](j, "name")).mapN((id, n) =>
      LinkTitleDto(id, n, Some(fileIcon(n, field[Boolean](j, "folder").contains(true))))
    )

  /** ファイル名の拡張子から選ぶアイコン。 */
  def fileIcon(name: String, folder: Boolean): String =
    if folder then "📁"
    else {
      val ext = name.split('.').toList match {
        case _ :: rest if rest.nonEmpty => rest.last.toLowerCase
        case _ => ""
      }
      ext match {
        case "png" | "jpg" | "jpeg" | "gif" | "webp" | "svg" | "bmp" | "heic" => "🖼️"
        case "pdf" => "📕"
        case "doc" | "docx" | "odt" | "rtf" => "📝"
        case "xls" | "xlsx" | "ods" | "csv" => "📊"
        case "ppt" | "pptx" | "odp" => "📽️"
        case "mp4" | "mov" | "webm" | "mkv" | "avi" => "🎬"
        case "mp3" | "wav" | "m4a" | "ogg" | "flac" => "🎵"
        case "zip" | "gz" | "tgz" | "7z" | "rar" | "tar" => "🗜️"
        case _ => "📄"
      }
    }

  val kinds: Map[String, Kind] = Map(
    "skiwi" -> Kind("skiwi", "read", Some(Lookup("/api/pages/lookup", "pages", skiwiPage))),
    "skawau" -> Kind("skawau", "read", Some(Lookup("/api/files/lookup", "files", skawauFile))),
    "stui" -> Kind("stui", "read write", None)
  )

  /** 環境変数を読む順。 */
  val providers: List[String] = List("skiwi", "skawau", "stui")
}
