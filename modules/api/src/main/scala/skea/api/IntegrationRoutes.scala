package skea.api

import sbirds.db.*
import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import sbirds.auth.Crypto
import skea.auth.SecretBox
import skea.core.*
import skea.db.*
import skea.integrations.ScmProvider
import skea.shared.*

import java.time.Instant
import java.util.UUID

/** 連携の設定（Webhook・自動化・リポジトリ）と、外部からの受信（SCM・自動化の Webhook）。 */
final class IntegrationRoutes(integ: IntegrationStore,
                              projects: ProjectStore,
                              users: UserStore,
                              activity: ActivityStore,
                              webhooks: WebhookService,
                              stui: StuiChannels,
                              automation: AutomationEngine,
                              scm: ScmService,
                              box: SecretBox,
                              val sessions: SessionAuth,
                              val access: Access2,
                              cfg: ApiConfig) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // ---------------------------------------------------------------- Webhook（送信）
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "webhooks" => getProjectWebhooks(req, pid)
    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "webhooks" => postProjectWebhooks(req, pid)
    case req @ PUT -> Root / "api" / "webhooks" / UuidVar(id) => putWebhook(req, id)
    case req @ GET -> Root / "api" / "integrations" / "stui" / "channels" => getIntegrationsStuiChannels(req)
    case req @ DELETE -> Root / "api" / "webhooks" / UuidVar(id) => deleteWebhook(req, id)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "webhooks" / "sample" =>
      getProjectWebhooksSample(req, pid)
    case req @ GET -> Root / "api" / "webhooks" / UuidVar(id) / "deliveries" => getWebhookDeliveries(req, id)
    case req @ POST -> Root / "api" / "webhooks" / UuidVar(id) / "test" => postWebhookTest(req, id)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "automations" => getProjectAutomations(req, pid)
    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "automations" => postProjectAutomations(req, pid)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "automation-runs" => getProjectAutomationRuns(req, pid)
    case req @ PUT -> Root / "api" / "automations" / UuidVar(id) => putAutomation(req, id)
    case req @ DELETE -> Root / "api" / "automations" / UuidVar(id) => deleteAutomation(req, id)
    case req @ POST -> Root / "hooks" / "automation" / token => postHooksAutomation(req, token)
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "repos" => getProjectRepos(req, pid)
    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "repos" => postProjectRepos(req, pid)
    case req @ PUT -> Root / "api" / "repos" / UuidVar(id) => putRepo(req, id)
    case req @ DELETE -> Root / "api" / "repos" / UuidVar(id) => deleteRepo(req, id)
    case req @ POST -> Root / "api" / "repos" / UuidVar(id) / "poll" => postRepoPoll(req, id)
    case req @ POST -> Root / "hooks" / "scm" / UuidVar(id) => postHooksScm(req, id)
  }

  private def audit(s: AuthSession, action: String, target: UUID, detail: Json = Json.obj()) =
    activity.audit(Some(s.user.id), s.viaString, action, Some(target.toString), detail)

  private def hookDto(w: WebhookRow): IO[WebhookDto] = {
    val stuiKind = WebhookKind.isStui(w.kind)
    w.ownerId.flatTraverse(users.find).map(o =>
      WebhookDto(
        w.id,
        w.name,
        if stuiKind then "" else w.url,
        if stuiKind then "" else w.secret,
        w.events,
        w.enabled,
        w.kind,
        w.channelId,
        w.channelName,
        o.map(u => UserBrief(u.id, u.name, u.avatarUrl))
      )
    )
  }

  /** 保存する内容（owner は stui の連携を使う人・受信 Webhook を作った人、hookId は stui の受信 Webhook、drop は消す古い受信 Webhook）。 */
  private final case class SavedHook(req: WebhookReq,
                                     owner: Option[UUID],
                                     hookId: Option[String],
                                     drop: Option[(UUID, String)])

  /** 古い設定が stui の受信 Webhook で、新しい設定で使わなくなるなら、それを消す（作った人の連携で）。 */
  private def dropOf(old: Option[WebhookRow]): Option[(UUID, String)] =
    old.filter(_.kind == WebhookKind.StuiHook).flatMap(o => (o.ownerId, o.stuiHookId).tupled)

  /**
   * 保存する内容を確かめて整える。
   *  - url: URL を確かめる
   *  - stui: 保存した人の連携で投稿する（保存した人を owner にする. 連携していなければ保存できない）
   *  - stui_hook: チャンネルの管理者が、連携で stui に受信 Webhook を作る（チャンネルが同じなら、前に作ったものをそのまま使う）
   */
  private def checkHook(s: AuthSession,
                        pid: UUID,
                        r: WebhookReq,
                        old: Option[WebhookRow]): IO[Either[String, SavedHook]] = {
    val events = r.events.filter(EventKind.all.contains)
    if r.isStui then {
      val ch = r.channelId.map(_.trim).filter(_.nonEmpty)
      val chName = r.channelName.map(_.trim.stripPrefix("#")).filter(_.nonEmpty)
      val kind = r.kind.getOrElse(WebhookKind.Stui)
      val base = r.copy(
        name = Option(r.name.trim).filter(_.nonEmpty).getOrElse("stui #" + chName.getOrElse("")),
        events = events,
        kind = Some(kind),
        channelId = ch,
        channelName = chName
      )
      val keep = old.filter(o =>
        kind == WebhookKind.StuiHook && o.kind == WebhookKind.StuiHook && o.channelId == ch && o.stuiHookId.isDefined
      )
      if !stui.configured then IO.pure(Left("stui との連携が設定されていません"))
      else if ch.isEmpty then IO.pure(Left("チャンネルを選んでください"))
      else
        keep match {
          case Some(o) => IO.pure(Right(SavedHook(base.copy(url = o.url), o.ownerId, o.stuiHookId, None)))
          case None =>
            stui.connected(s.user.id).flatMap {
              case false => IO.pure(Left("stui と連携してから保存してください"))
              case true if kind == WebhookKind.Stui =>
                IO.pure(Right(SavedHook(base.copy(url = ""), Some(s.user.id), None, dropOf(old))))
              case true =>
                projects.find(pid).flatMap(p =>
                  stui.createHook(s.user.id, ch.get, p.fold("")(x => s"${x.key} ${x.name}"))
                ).map(_.map((hid, url) =>
                  SavedHook(base.copy(url = url), Some(s.user.id), Some(hid), dropOf(old))
                ))
            }
        }
    } else
      webhooks.validUrl(r.url) match {
        case Left(f) => IO.pure(Left(f.message))
        case Right(_) if r.name.trim.isEmpty => IO.pure(Left("名前を入力してください"))
        case Right(u) => NetGuard.check(u).map(_.map(_ =>
            SavedHook(
              r.copy(url = u, events = events, kind = Some(WebhookKind.Url), channelId = None, channelName = None),
              None,
              None,
              dropOf(old)
            )
          ))
      }
  }

  private def dropHook(d: Option[(UUID, String)]): IO[Unit] = d.traverse_((owner, hid) => stui.deleteHook(owner, hid))

  private def ruleDto(r: RuleRow): IO[RuleDto] =
    r.runAs.flatTraverse(users.find).map(u =>
      RuleDto(
        r.id,
        r.name,
        r.enabled,
        r.trigger,
        r.actions,
        u.map(x => UserBrief(x.id, x.name, x.avatarUrl)),
        r.incomingToken.map(t => s"${cfg.publicUrl}/hooks/automation/$t"),
        r.lastRunAt,
        r.createdAt
      )
    )

  private def repoDto(r: RepoRow) =
    RepoDto(
      r.id,
      r.provider,
      r.name,
      r.webUrl,
      r.apiUrl,
      r.branch,
      s"${cfg.publicUrl}/hooks/scm/${r.id}",
      r.secret,
      r.tokenEnc.isDefined,
      r.lastPolledAt,
      r.allRepos
    )

  /** 実行するユーザーは、自分か（空なら作成者）、もとから設定されている人だけ。他人を指定できるのはシステム管理者だけ。 */
  private def httpUrl(u: String): Boolean = {
    val t = u.trim.toLowerCase; t.startsWith("https://") || t.startsWith("http://")
  }

  private def ruleOk(s: AuthSession, r: RuleReq, current: Option[UUID] = None): Either[String, Unit] =
    if r.name.trim.isEmpty then Left("名前を入力してください")
    else if r.runAs.exists(u => u != s.user.id && !current.contains(u) && !s.canAdmin) then
      Left("実行するユーザーには自分だけを指定できます")
    else Automation.validate(r.trigger, r.actions)

  /** プロジェクトに属する設定を読み、そのプロジェクトの管理権限を確認してから処理する（見えなければ 404）。 */
  private def withItem[A](req: Request[IO],
                          find: IO[Option[A]],
                          what: String)(pid: A => UUID)(f: (AuthSession, A) => IO[Response[IO]]) =
    authed(req)(s =>
      find.flatMap {
        case None => err(ErrorCode.ItemNotFound, s"$what が見つかりません")
        case Some(a) => respond(access.project(s, pid(a), Role.Admin))(_ => f(s, a))
      }
    )

  private def getProjectWebhooks(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((_, _) => integ.webhooks(pid).flatMap(_.traverse(hookDto)).flatMap(Ok(_)))

  private def postProjectWebhooks(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((s, _) =>
      body[WebhookReq](req)(r =>
        checkHook(s, pid, r, None).flatMap {
          case Left(m) => err(ErrorCode.InvalidInput, m)
          case Right(SavedHook(x, owner, hid, _)) =>
            integ.createWebhook(pid, x, Crypto.randomToken(24), owner, hid).flatMap(id =>
              audit(
                s,
                "webhook.create",
                pid,
                Json.obj("kind" -> x.kind.asJson, "url" -> x.url.asJson, "channel" -> x.channelName.asJson)
              ) *>
                integ.webhook(id).flatMap(_.traverse(hookDto)).flatMap(Created(_))
            )
        }
      )
    )

  private def putWebhook(req: Request[IO], id: UUID): IO[Response[IO]] =
    withItem(req, integ.webhook(id), "Webhook")(_.projectId)((s, w) =>
      body[WebhookReq](req)(r =>
        checkHook(s, w.projectId, r, Some(w)).flatMap {
          case Left(m) => err(ErrorCode.InvalidInput, m)
          case Right(SavedHook(x, owner, hid, drop)) =>
            integ.updateWebhook(id, x, owner, hid) *> dropHook(drop) *> audit(s, "webhook.update", w.projectId) *>
              NoContent()
        }
      )
    )

  private def getIntegrationsStuiChannels(req: Request[IO]): IO[Response[IO]] =
    authed(req)(s =>
      stui.channels(s.user.id).flatMap {
        case Left(f) => fail(f)
        case Right(cs) => Ok(cs)
      }
    )

  private def deleteWebhook(req: Request[IO], id: UUID): IO[Response[IO]] =
    withItem(req, integ.webhook(id), "Webhook")(_.projectId)((s, w) =>
      integ.deleteWebhook(id) *> dropHook(dropOf(Some(w))) *> audit(s, "webhook.delete", w.projectId) *> NoContent()
    )

  private def getProjectWebhooksSample(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((s, _) =>
      projects.find(pid).flatMap {
        case None => err(ErrorCode.ProjectNotFound)
        case Some(p) =>
          val kind = param(req, "event").filter(EventKind.all.contains).getOrElse(EventKind.TaskUpdated)
          Ok(EventHub.sample(kind, p, (s.user.id, s.user.name)))
      }
    )

  private def getWebhookDeliveries(req: Request[IO], id: UUID): IO[Response[IO]] =
    withItem(req, integ.webhook(id), "Webhook")(_.projectId)((_, _) => integ.deliveries(id, 50).flatMap(Ok(_)))

  private def postWebhookTest(req: Request[IO], id: UUID): IO[Response[IO]] =
    withItem(req, integ.webhook(id), "Webhook")(_.projectId)((s, w) =>
      projects.find(w.projectId).flatMap { p =>
        val payload =
          if WebhookKind.isStui(w.kind) then
            EventHub.payload("ping", p, None, Some((s.user.id, s.user.name)), Json.obj())
          else Json.obj("event" -> "ping".asJson, "at" -> Instant.now().asJson, "actor" -> s.user.name.asJson)
        webhooks.send(DeliveryRow(
          UUID.randomUUID(),
          w.id,
          w.url,
          w.secret,
          "ping",
          payload,
          0,
          w.kind,
          w.ownerId,
          w.channelId
        ))
      }.flatMap {
        (r, code) =>
          Ok(Json.obj(
            "ok" -> (r == OutboxResult.Sent).asJson,
            "statusCode" -> code.asJson,
            "result" -> r.toString.asJson
          ))
      }
    )

  private def getProjectAutomations(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((_, _) => integ.rules(pid).flatMap(_.traverse(ruleDto)).flatMap(Ok(_)))

  private def postProjectAutomations(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((s, _) =>
      body[RuleReq](req)(r =>
        ruleOk(s, r) match {
          case Left(e) => err(ErrorCode.InvalidInput, e)
          case Right(_) =>
            val token = Option.when(
              r.trigger.hcursor.get[String]("type").contains("incoming_webhook")
            )(Crypto.randomToken(24))
            integ.createRule(pid, r.copy(name = r.name.trim), s.user.id, token).flatMap(id =>
              audit(s, "automation.create", pid, Json.obj("name" -> r.name.asJson)) *>
                integ.rule(id).flatMap(_.traverse(ruleDto)).flatMap(Created(_))
            )
        }
      )
    )

  private def getProjectAutomationRuns(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((_, _) =>
      integ.runs(pid, param(req, "ruleId").flatMap(uuidOpt), intParam(req, "limit", 50, 200)).flatMap(Ok(_))
    )

  private def putAutomation(req: Request[IO], id: UUID): IO[Response[IO]] =
    withItem(req, integ.rule(id), "ルール")(_.projectId)((s, old) =>
      body[RuleReq](req)(r =>
        ruleOk(s, r, old.runAs) match {
          case Left(e) => err(ErrorCode.InvalidInput, e)
          case Right(_) =>
            val token = Option.when(
              r.trigger.hcursor.get[String]("type").contains("incoming_webhook")
            )(Crypto.randomToken(24))
            integ.updateRule(id, r.copy(name = r.name.trim), token) *>
              audit(s, "automation.update", old.projectId) *> NoContent()
        }
      )
    )

  private def deleteAutomation(req: Request[IO], id: UUID): IO[Response[IO]] =
    withItem(req, integ.rule(id), "ルール")(_.projectId)((s, r) =>
      integ.deleteRule(id) *> audit(s, "automation.delete", r.projectId) *> NoContent()
    )

  private def postHooksAutomation(req: Request[IO], token: String): IO[Response[IO]] =
    req.bodyText.compile.string.flatMap { b =>
      val j = if b.trim.isEmpty then Json.obj() else io.circe.parser.parse(b).getOrElse(Json.fromString(b))
      respond(automation.incoming(token, j))(n => Accepted(Json.obj("queued" -> n.asJson)))
    }

  private def getProjectRepos(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.View)((_, r) =>
      integ.repos(pid).flatMap(rs =>
        Ok(rs.map(repoDto).map(d => if r >= Role.Admin then d else d.copy(secret = "")))
      )
    )

  private def postProjectRepos(req: Request[IO], pid: UUID): IO[Response[IO]] =
    withProject(req, pid, Role.Admin)((s, _) =>
      body[RepoReq](req)(r =>
        ScmProvider.parse(r.provider) match {
          case None => err(ErrorCode.ScmProviderInvalid)
          case Some(_) if (r.name.trim.isEmpty && !r.allRepos.contains(true)) || !httpUrl(r.webUrl) =>
            err(ErrorCode.ScmRepoNameAndUrlRequired)
          case Some(_) if r.apiUrl.exists(u => u.trim.nonEmpty && !httpUrl(u)) =>
            err(ErrorCode.ScmApiUrlInvalid)
          case Some(p) =>
            integ.createRepo(
              pid,
              r.copy(provider = p.asString, name = r.name.trim),
              Crypto.randomToken(24),
              r.token.filter(_.trim.nonEmpty).map(box.encrypt)
            )
              .flatMap(id =>
                audit(s, "repo.create", pid, Json.obj("name" -> r.name.asJson)) *>
                  integ.repo(id).flatMap(x => Created(x.map(repoDto)))
              )
        }
      )
    )

  private def putRepo(req: Request[IO], id: UUID): IO[Response[IO]] =
    withItem(req, integ.repo(id), "リポジトリ")(_.projectId)((s, old) =>
      body[RepoReq](req)(r =>
        ScmProvider.parse(r.provider) match {
          case None => err(ErrorCode.ScmProviderInvalid)
          case Some(_) if !httpUrl(r.webUrl) || r.apiUrl.exists(u => u.trim.nonEmpty && !httpUrl(u)) =>
            err(ErrorCode.UrlSchemeInvalid)
          case Some(p) =>
            // 送り先（プロバイダ・URL）を変えたら、保存してあるトークンは捨てる（別のサーバーへトークンを送らせない）
            val moved = old.provider != p.asString || old.webUrl != r.webUrl ||
              old.apiUrl.filter(_.trim.nonEmpty) != r.apiUrl.filter(_.trim.nonEmpty)
            val newToken = r.token.filter(_.trim.nonEmpty)
            integ.updateRepo(
              id,
              r.copy(provider = p.asString),
              newToken.map(box.encrypt),
              r.token.contains("") || (moved && newToken.isEmpty)
            ) *>
              audit(s, "repo.update", old.projectId) *> NoContent()
        }
      )
    )

  private def deleteRepo(req: Request[IO], id: UUID): IO[Response[IO]] =
    withItem(req, integ.repo(id), "リポジトリ")(_.projectId)((s, r) =>
      integ.deleteRepo(id) *> audit(s, "repo.delete", r.projectId) *> NoContent()
    )

  private def postRepoPoll(req: Request[IO], id: UUID): IO[Response[IO]] =
    withItem(req, integ.repo(id), "リポジトリ")(_.projectId)((_, r) =>
      respond(scm.poll(r))(n => Ok(Json.obj("linked" -> n.asJson)))
    )

  private def postHooksScm(req: Request[IO], id: UUID): IO[Response[IO]] =
    req.body.compile.to(Array).flatMap(b =>
      respond(scm.receive(id, req.headers, b))(n => Ok(Json.obj("linked" -> n.asJson)))
    )
}
