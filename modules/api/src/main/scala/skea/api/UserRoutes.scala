package skea.api

import cats.effect.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import sbirds.auth.Crypto
import skea.auth.ApiKeys
import skea.core.*
import skea.db.*
import skea.shared.*

import cats.syntax.all.*
import org.http4s.headers.`Content-Type`
import sbirds.storage.BlobStore

import java.time.Instant
import java.util.UUID

/** 自分の情報・APIキー・通知の設定、ユーザーの一覧、管理画面（ユーザー・エージェント専用アカウント・監査ログ）。 */
final class UserRoutes(users: UserStore,
                       projects: ProjectStore,
                       activity: ActivityStore,
                       blobs: Option[BlobStore],
                       emoji: EmojiStore,
                       val sessions: SessionAuth,
                       val access: Access2) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "me" => getMe(req)
    case req @ PATCH -> Root / "api" / "me" => patchMe(req)
    case req @ GET -> Root / "api" / "me" / "notify-prefs" => getMeNotifyPrefs(req)
    case req @ PUT -> Root / "api" / "me" / "notify-prefs" => putMeNotifyPrefs(req)
    case req @ GET -> Root / "api" / "me" / "api-keys" => getMeApiKeys(req)
    case req @ POST -> Root / "api" / "me" / "api-keys" => postMeApiKeys(req)
    case req @ DELETE -> Root / "api" / "me" / "api-keys" / UuidVar(id) => deleteMeApiKey(req, id)
    case req @ POST -> Root / "api" / "me" / "avatar" => postMeAvatar(req)
    case req @ DELETE -> Root / "api" / "me" / "avatar" => deleteMeAvatar(req)
    case req @ GET -> Root / "api" / "users" / UuidVar(id) / "avatar" => getUserAvatar(req, id)
    case req @ GET -> Root / "api" / "emoji" => getEmoji(req)
    case req @ POST -> Root / "api" / "emoji" => postEmoji(req)
    case req @ DELETE -> Root / "api" / "emoji" / name => deleteEmoji(req, name)
    case req @ GET -> Root / "api" / "emoji" / name / "image" => getEmojiImage(req, name)
    case req @ GET -> Root / "api" / "users" => getUsers(req)
    case req @ GET -> Root / "api" / "admin" / "users" => getAdminUsers(req)
    case req @ PATCH -> Root / "api" / "admin" / "users" / UuidVar(id) => patchAdminUser(req, id)
    case req @ GET -> Root / "api" / "admin" / "agents" => getAdminAgents(req)
    case req @ POST -> Root / "api" / "admin" / "agents" => postAdminAgents(req)
    case req @ PATCH -> Root / "api" / "admin" / "agents" / UuidVar(id) => patchAdminAgent(req, id)
    case req @ POST -> Root / "api" / "admin" / "agents" / UuidVar(id) / "keys" => postAdminAgentKeys(req, id)
    case req @ DELETE -> Root / "api" / "admin" / "agents" / UuidVar(id) / "keys" / UuidVar(kid) =>
      deleteAdminAgentKey(req, id, kid)
    case req @ GET -> Root / "api" / "admin" / "audit" => getAdminAudit(req)
  }

  private val avatarTypes = Set("image/png", "image/jpeg", "image/gif", "image/webp")
  private val avatarMaxBytes = 2L * 1024 * 1024

  /** アイコンを差し替える（前の画像は消す）。 */
  private def replaceAvatar(uid: UUID, key: Option[String]): IO[Unit] = {
    val url = key.map(_ => s"/api/users/$uid/avatar?v=${Crypto.randomToken(6)}")
    users.setAvatar(uid, key, url).flatMap(_.traverse_(old => blobs.traverse_(_.delete(old).handleError(_ => ()))))
  }

  private def me(s: AuthSession): MeDto =
    MeDto(
      s.user.id,
      s.user.name,
      s.user.email,
      s.user.avatarUrl,
      s.isAdmin,
      s.user.timezone,
      s.viaString,
      s.scopeStrings
    )

  /**
   * APIキーを発行する（個人設定の自分のキー・管理画面のエージェント専用アカウントのキーで共通）。owner はキーの持ち主。
   * adminDenied: admin スコープを求められたときに返す応答（None なら許す）。平文のキーはこの応答でだけ返す。
   */
  private def issueKey(owner: AuthSession,
                       r: CreateApiKeyReq,
                       adminDenied: Option[IO[Response[IO]]])(audit: ApiKeyDto => IO[Unit]): IO[Response[IO]] = {
    val scopes = r.scopes.map(_.trim.toLowerCase).distinct
    if r.name.trim.isEmpty then err(ErrorCode.NameRequired)
    else if scopes.isEmpty || !scopes.forall(ApiScope.all.contains) then
      err(ErrorCode.ApiKeyScopeInvalid)
    else if scopes.contains(ApiScope.Admin) && adminDenied.isDefined then adminDenied.get
    else
      projects.visible(owner).flatMap { vis =>
        val ps = r.projectIds.map(_.distinct.filter(vis.contains)).filter(_.nonEmpty)
        if r.projectIds.exists(_.nonEmpty) && ps.isEmpty then err(ErrorCode.ApiKeyProjectRequired)
        else {
          val (key, prefix) = ApiKeys.generate()
          val exp = r.expiresInDays.filter(_ > 0).map(d => Instant.now().plusSeconds(d.toLong * 86400))
          users.createKey(owner.user.id, r.name.trim, prefix, Crypto.sha256(key), scopes, ps, exp).flatMap(info =>
            audit(info) *> Created(CreatedApiKeyDto(key, info))
          )
        }
      }
  }

  /** 管理画面のエージェント専用アカウントの操作（管理者・画面からのみ。APIキーでは管理できない）。 */
  private def agentAdmin(req: Request[IO])(f: AuthSession => IO[Response[IO]]): IO[Response[IO]] =
    admin(req)(s => if s.isSession then f(s) else err(ErrorCode.SessionOnly))

  /** id がエージェント専用アカウントのときだけ処理する（ほかのユーザーは 404）。 */
  private def withAgent(id: UUID)(f: => IO[Response[IO]]): IO[Response[IO]] =
    users.isAgent(id).ifM(f, err(ErrorCode.AgentAccountNotFound))

  private def agentAudit(s: AuthSession, action: String, target: UUID, detail: Json): IO[Unit] =
    activity.audit(Some(s.user.id), s.viaString, action, Some(target.toString), detail)

  private def getMe(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s => sessions.appNames(req).flatMap(n => Ok(me(s).copy(appNames = n))))

  private def patchMe(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s =>
      body[UpdateMeReq](req)(r =>
        if r.timezone.exists(tz => scala.util.Try(java.time.ZoneId.of(tz)).isFailure) then
          err(ErrorCode.TimezoneInvalid)
        else users.updateMe(s.user.id, r.name, r.timezone) *> NoContent()
      )
    )

  private def getMeNotifyPrefs(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s => users.notifyPrefs(s.user.id).flatMap(Ok(_)))

  private def putMeNotifyPrefs(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s => body[List[NotifyPrefDto]](req)(ps => users.setNotifyPrefs(s.user.id, ps) *> NoContent()))

  private def getMeApiKeys(req: Request[IO]): IO[Response[IO]] =
    sessionOnly(req)(s => users.keys(s.user.id).flatMap(Ok(_)))

  private def postMeApiKeys(req: Request[IO]): IO[Response[IO]] =
    sessionOnly(req)(s =>
      body[CreateApiKeyReq](req)(r =>
        issueKey(s, r, Option.when(!s.isAdmin)(err(ErrorCode.ApiKeyAdminScopeAdminOnly)))(info =>
          activity.audit(
            Some(s.user.id),
            "web",
            "api_key.create",
            Some(info.id.toString),
            Json.obj("name" -> info.name.asJson, "scopes" -> info.scopes.asJson)
          )
        )
      )
    )

  private def deleteMeApiKey(req: Request[IO], id: UUID): IO[Response[IO]] =
    sessionOnly(req)(s =>
      users.revokeKey(s.user.id, id).flatMap {
        case true => activity.audit(Some(s.user.id), "web", "api_key.revoke", Some(id.toString), Json.obj()) *>
            NoContent()
        case false => err(ErrorCode.ApiKeyNotFound)
      }
    )

  private def postMeAvatar(req: Request[IO]): IO[Response[IO]] =
    sessionOnly(req)(s =>
      blobs match {
        case None => err(ErrorCode.ImageStorageNotConfigured)
        case Some(store) =>
          withUpload(req, avatarMaxBytes)(up =>
            if !avatarTypes.contains(up.contentType) then err(ErrorCode.ImageTypeInvalid)
            else {
              val key = s"avatars/${s.user.id}/${UUID.randomUUID()}"
              store.put(key, up.path, up.contentType) *> replaceAvatar(s.user.id, Some(key)) *>
                users.find(s.user.id).flatMap(u => Ok(Json.obj("avatarUrl" -> u.flatMap(_.avatarUrl).asJson)))
            }
          )
      }
    )

  private def deleteMeAvatar(req: Request[IO]): IO[Response[IO]] =
    sessionOnly(req)(s => replaceAvatar(s.user.id, None) *> NoContent())

  private def getUserAvatar(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req)(_ =>
      (users.avatarKey(id), IO.pure(blobs)).tupled.flatMap {
        case (Some(key), Some(store)) => store.get(key).flatMap {
            case None => err(ErrorCode.ImageNotFound)
            case Some((meta, bytes)) =>
              val mt = MediaType.parse(meta.contentType).toOption.filter(m =>
                avatarTypes(s"${m.mainType}/${m.subType}")
              ).getOrElse(MediaType.image.png)
              IO.pure(Response[IO](Status.Ok).withEntity(Entity.stream(bytes, Some(meta.size)))
                .putHeaders(
                  `Content-Type`(mt),
                  "X-Content-Type-Options" -> "nosniff",
                  "Cache-Control" -> "private, max-age=86400"
                ))
          }
        case _ => err(ErrorCode.ImageNotRegistered)
      }
    )

  private def getEmoji(req: Request[IO]): IO[Response[IO]] =
    authed(req)(_ => emoji.list.flatMap(Ok(_)))

  private def postEmoji(req: Request[IO]): IO[Response[IO]] =
    sessionOnly(req)(s =>
      (blobs, param(req, "name").map(_.toLowerCase)) match {
        case (None, _) => err(ErrorCode.ImageStorageNotConfigured)
        case (_, n) if !n.exists(_.matches("^[a-z0-9_+-]{2,32}$")) =>
          err(ErrorCode.EmojiNameInvalid)
        case (Some(store), Some(name)) =>
          withUpload(req, 512L * 1024)(up =>
            if !avatarTypes.contains(up.contentType) then err(ErrorCode.ImageTypeInvalid)
            else {
              val key = s"emoji/$name-${UUID.randomUUID()}"
              store.put(key, up.path, up.contentType) *> emoji.create(name, key, up.contentType, s.user.id).flatMap {
                case true => Created(CustomEmojiDto(name, CustomEmoji.url(name), Some(s.user.name)))
                case false => store.delete(key).handleError(_ => ()) *>
                    err(ErrorCode.EmojiNameTaken, s"「:$name:」はすでにあります")
              }
            }
          )
        case _ => err(ErrorCode.EmojiNameRequired)
      }
    )

  private def deleteEmoji(req: Request[IO], name: String): IO[Response[IO]] =
    sessionOnly(req)(s =>
      emoji.find(name).flatMap {
        case None => err(ErrorCode.EmojiNotFound)
        case Some(e) if !e.createdBy.contains(s.user.id) && !s.isAdmin => err(ErrorCode.EmojiDeleteForbidden)
        case Some(e) => emoji.delete(name) *> blobs.traverse_(_.delete(e.s3Key).handleError(_ => ())) *> NoContent()
      }
    )

  private def getEmojiImage(req: Request[IO], name: String): IO[Response[IO]] =
    authed(req)(_ =>
      (emoji.find(name), IO.pure(blobs)).tupled.flatMap {
        case (Some(e), Some(store)) => store.get(e.s3Key).flatMap {
            case None => err(ErrorCode.ImageNotFound)
            case Some((meta, bytes)) =>
              val mt = MediaType.parse(e.contentType).toOption.getOrElse(MediaType.image.png)
              IO.pure(Response[IO](Status.Ok).withEntity(Entity.stream(bytes, Some(meta.size)))
                .putHeaders(
                  `Content-Type`(mt),
                  "X-Content-Type-Options" -> "nosniff",
                  "Cache-Control" -> "private, max-age=86400"
                ))
          }
        case _ => err(ErrorCode.EmojiNotFound)
      }
    )

  private def getUsers(req: Request[IO]): IO[Response[IO]] =
    authed(req)(_ =>
      users.list(param(req, "q"), includeDisabled = false, intParam(req, "limit", 50, 500)).flatMap(us =>
        Ok(us.map(u => UserBrief(u.id, u.name, u.avatarUrl)))
      )
    )

  private def getAdminUsers(req: Request[IO]): IO[Response[IO]] =
    admin(req)(_ => users.list(param(req, "q"), includeDisabled = true, 2000).flatMap(Ok(_)))

  private def patchAdminUser(req: Request[IO], id: UUID): IO[Response[IO]] =
    admin(req)(s =>
      body[UpdateUserReq](req) { r =>
        if id == s.user.id && (r.isAdmin.contains(false) || r.disabled.contains(true)) then
          err(ErrorCode.AdminSelfDemote)
        else
          users.update(id, r.isAdmin, r.disabled).flatMap {
            case true => activity.audit(Some(s.user.id), s.viaString, "user.update", Some(id.toString), r.asJson) *>
                NoContent()
            case false => err(ErrorCode.UserNotFound)
          }
      }
    )

  private def getAdminAgents(req: Request[IO]): IO[Response[IO]] =
    agentAdmin(req)(_ => users.agents.flatMap(Ok(_)))

  private def postAdminAgents(req: Request[IO]): IO[Response[IO]] =
    agentAdmin(req)(s =>
      body[CreateAgentReq](req)(r =>
        AgentAccounts.validName(r.name) match {
          case Left(m) => err(ErrorCode.InvalidInput, m)
          case Right(name) =>
            users.createAgent(name).flatTap(id =>
              agentAudit(s, "agent.create", id, Json.obj("name" -> name.asJson))
            ).flatMap(id =>
              users.agents.flatMap(as =>
                as.find(_.id == id).fold(err(ErrorCode.AgentAccountNotFound))(Created(_))
              )
            )
        }
      )
    )

  private def patchAdminAgent(req: Request[IO], id: UUID): IO[Response[IO]] =
    agentAdmin(req)(s =>
      body[UpdateAgentReq](req)(r =>
        r.name.traverse(AgentAccounts.validName) match {
          case Left(m) => err(ErrorCode.InvalidInput, m)
          case Right(name) => users.updateAgent(id, name, r.disabled).flatMap {
              case true => agentAudit(s, "agent.update", id, UpdateAgentReq(name, r.disabled).asJson) *> NoContent()
              case false => err(ErrorCode.AgentAccountNotFound)
            }
        }
      )
    )

  private def postAdminAgentKeys(req: Request[IO], id: UUID): IO[Response[IO]] =
    agentAdmin(req)(s =>
      withAgent(id)(body[CreateApiKeyReq](req)(r =>
        sessions.sessionFor(id).flatMap {
          case None => err(ErrorCode.AgentKeyForDisabled)
          case Some(agent) =>
            issueKey(agent, r, Some(err(ErrorCode.AgentKeyAdminScope)))(info =>
              agentAudit(
                s,
                "agent.key.create",
                id,
                Json.obj("key" -> info.id.asJson, "name" -> info.name.asJson, "scopes" -> info.scopes.asJson)
              )
            )
        }
      ))
    )

  private def deleteAdminAgentKey(req: Request[IO], id: UUID, kid: UUID): IO[Response[IO]] =
    agentAdmin(req)(s =>
      withAgent(id)(users.revokeKey(id, kid).flatMap {
        case true => agentAudit(s, "agent.key.revoke", id, Json.obj("key" -> kid.asJson)) *> NoContent()
        case false => err(ErrorCode.ApiKeyNotFound)
      })
    )

  private def getAdminAudit(req: Request[IO]): IO[Response[IO]] =
    admin(req)(_ =>
      activity.auditLog(
        param(req, "before").flatMap(_.toLongOption),
        param(req, "action"),
        intParam(req, "limit", 100, 500)
      ).flatMap(Ok(_))
    )
}
