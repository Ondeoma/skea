package skea.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skea.auth.{Crypto, SecretBox}
import skea.core.*
import skea.db.*
import skea.integrations.ScmProvider
import skea.shared.*

import java.time.Instant
import java.util.UUID

/** 検索・保存した検索条件・通知。 */
final class SearchRoutes(search: SearchStore, projects: ProjectStore, activity: ActivityStore, val sessions: SessionAuth, val access: Access2)
    extends RouteSupport:

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "search" =>
      authed(req) { s =>
        SearchQuery.parse(param(req, "q").getOrElse("")) match
          case Left(e) => Ok(SearchResultDto(Nil, 0, Some(e)))
          case Right(q) =>
            projects.visible(s).flatMap { vis =>
              val ps = param(req, "projectId").flatMap(uuidOpt).fold(vis.keys.toList)(p => vis.keys.filter(_ == p).toList)
              search.search(q, search.Ctx(s.user.id, ps, Op(s).today), intParam(req, "limit", 50, 500), param(req, "offset").flatMap(_.toIntOption).getOrElse(0))
                .flatMap {
                  case Left(e)  => Ok(SearchResultDto(Nil, 0, Some(e)))
                  case Right(r) => Ok(r)
                }
            }
      }

    case req @ GET -> Root / "api" / "filters" => authed(req)(s => search.savedFilters(s.user.id).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "filters" =>
      authed(req)(s => body[SavedFilterReq](req)(r =>
        if r.name.trim.isEmpty || r.query.trim.isEmpty then err(Status.BadRequest, "名前と条件を入力してください")
        else search.saveFilter(s.user.id, r.name.trim, r.query.trim).flatMap(Created(_))))

    case req @ DELETE -> Root / "api" / "filters" / UuidVar(id) => authed(req)(s => search.deleteFilter(s.user.id, id) *> NoContent())

    // ---------------------------------------------------------------- ボードの絞り込み条件
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "board-filters" =>
      withProject(req, pid, Role.View)((s, _) => search.boardFilters(pid, s.user.id).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "board-filters" =>
      // 自分用は閲覧権限で、プロジェクトで共有するものは編集権限で保存できる
      withProject(req, pid, Role.View)((s, role) => body[BoardFilterReq](req)(r =>
        if r.name.trim.isEmpty || r.name.length > 50 then err(Status.BadRequest, "名前を 50 文字までで入力してください")
        else if r.cond.isEmpty then err(Status.BadRequest, "絞り込みの条件がありません")
        else if r.cond.productIterator.exists(_.toString.length > 200) then err(Status.BadRequest, "条件が長すぎます")
        else if r.shared && role < Role.Edit then err(Status.Forbidden, "共有するには編集権限が必要です")
        else search.saveBoardFilter(pid, s.user.id, r).orFail.flatMap(_.fold(fail, Created(_)))))

    case req @ DELETE -> Root / "api" / "board-filters" / UuidVar(id) =>
      authed(req)(s => search.boardFilter(id).flatMap {
        case Some((pid, Some(uid), _)) if uid == s.user.id =>
          respond(access.project(s, pid, Role.View))(_ => search.deleteBoardFilter(id) *> NoContent())
        case Some((pid, None, by)) =>
          // 共有のものは、作った人か、プロジェクトの管理者が消せる
          respond(access.project(s, pid, if by.contains(s.user.id) then Role.Edit else Role.Admin))(_ => search.deleteBoardFilter(id) *> NoContent())
        case _ => err(Status.NotFound, "条件が見つかりません")
      })

    // ---------------------------------------------------------------- 通知
    case req @ GET -> Root / "api" / "notifications" =>
      authed(req) { s =>
        val kinds = param(req, "kinds").toList.flatMap(_.split(',').toList.map(_.trim)).filter(NotificationKind.all.contains)
        val before = param(req, "before").flatMap(b => scala.util.Try(Instant.parse(b)).toOption)
        // ヘルプデスクの"ユーザ"として参加しているプロジェクトの通知も出す
        projects.memberships(s).flatMap(vis =>
          activity.list(s.user.id, kinds, param(req, "unread").contains("true"), vis.keys.toList, before, intParam(req, "limit", 50, 200)).flatMap(Ok(_)))
      }

    case req @ GET -> Root / "api" / "notifications" / "unread" => authed(req)(s => activity.unread(s.user.id).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "notifications" / "read" =>
      authed(req)(s => body[MarkReadReq](req)(r => activity.markRead(s.user.id, r.ids) *> NoContent()))

    case req @ POST -> Root / "api" / "notifications" / "read-all" =>
      authed(req)(s => activity.markAllRead(s.user.id, param(req, "kinds").toList.flatMap(_.split(',').toList)) *> NoContent())
  }

/** 連携の設定（Webhook・自動化・リポジトリ）と、外部からの受信（SCM・自動化の Webhook）。 */
final class IntegrationRoutes(
    integ: IntegrationStore,
    projects: ProjectStore,
    users: UserStore,
    activity: ActivityStore,
    webhooks: WebhookService,
    automation: AutomationEngine,
    scm: ScmService,
    box: SecretBox,
    val sessions: SessionAuth,
    val access: Access2,
    cfg: ApiConfig
) extends RouteSupport:

  private def audit(s: AuthSession, action: String, target: UUID, detail: Json = Json.obj()) =
    activity.audit(Some(s.user.id), s.viaString, action, Some(target.toString), detail)

  private def hookDto(w: WebhookRow) = WebhookDto(w.id, w.name, w.url, w.secret, w.events, w.enabled)

  private def ruleDto(r: RuleRow): IO[RuleDto] =
    r.runAs.flatTraverse(users.find).map(u => RuleDto(r.id, r.name, r.enabled, r.trigger, r.actions, u.map(x => UserBrief(x.id, x.name, x.avatarUrl)),
      r.incomingToken.map(t => s"${cfg.publicUrl}/hooks/automation/$t"), r.lastRunAt, r.createdAt))

  private def repoDto(r: RepoRow) =
    RepoDto(r.id, r.provider, r.name, r.webUrl, r.apiUrl, r.branch, s"${cfg.publicUrl}/hooks/scm/${r.id}", r.secret, r.tokenEnc.isDefined, r.lastPolledAt,
      r.allRepos)

  /** 実行するユーザーは、自分か（空なら作成者）、もとから設定されている人だけ。他人を指定できるのはシステム管理者だけ。 */
  private def httpUrl(u: String): Boolean = { val t = u.trim.toLowerCase; t.startsWith("https://") || t.startsWith("http://") }

  private def ruleOk(s: AuthSession, r: RuleReq, current: Option[UUID] = None): Either[String, Unit] =
    if r.name.trim.isEmpty then Left("名前を入力してください")
    else if r.runAs.exists(u => u != s.user.id && !current.contains(u) && !s.canAdmin) then Left("実行するユーザーには自分だけを指定できます")
    else Automation.validate(r.trigger, r.actions)

  /** プロジェクトに属する設定を読み、そのプロジェクトの管理権限を確認してから処理する（見えなければ 404）。 */
  private def withItem[A](req: Request[IO], find: IO[Option[A]], what: String)(pid: A => UUID)(f: (AuthSession, A) => IO[Response[IO]]) =
    authed(req)(s => find.flatMap {
      case None => err(Status.NotFound, s"$what が見つかりません")
      case Some(a) => respond(access.project(s, pid(a), Role.Admin))(_ => f(s, a))
    })

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // ---------------------------------------------------------------- Webhook（送信）
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "webhooks" =>
      withProject(req, pid, Role.Admin)((_, _) => integ.webhooks(pid).flatMap(ws => Ok(ws.map(hookDto))))

    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "webhooks" =>
      withProject(req, pid, Role.Admin)((s, _) => body[WebhookReq](req)(r => webhooks.validUrl(r.url) match
        case Left(f) => fail(f)
        case Right(_) if r.name.trim.isEmpty => err(Status.BadRequest, "名前を入力してください")
        case Right(u) => NetGuard.check(u).flatMap {
          case Left(m) => err(Status.BadRequest, m)
          case Right(_) =>
          integ.createWebhook(pid, r.copy(url = u, events = r.events.filter(EventKind.all.contains)), Crypto.randomToken(24)).flatMap(id =>
            audit(s, "webhook.create", pid, Json.obj("url" -> u.asJson)) *> integ.webhook(id).flatMap(w => Created(w.map(hookDto))))
        }))

    case req @ PUT -> Root / "api" / "webhooks" / UuidVar(id) =>
      withItem(req, integ.webhook(id), "Webhook")(_.projectId)((s, w) => body[WebhookReq](req)(r => webhooks.validUrl(r.url) match
        case Left(f) => fail(f)
        case Right(u) => NetGuard.check(u).flatMap {
          case Left(m) => err(Status.BadRequest, m)
          case Right(_) => integ.updateWebhook(id, r.copy(url = u, events = r.events.filter(EventKind.all.contains))) *>
              audit(s, "webhook.update", w.projectId) *> NoContent()
        }))

    case req @ DELETE -> Root / "api" / "webhooks" / UuidVar(id) =>
      withItem(req, integ.webhook(id), "Webhook")(_.projectId)((s, w) => integ.deleteWebhook(id) *> audit(s, "webhook.delete", w.projectId) *> NoContent())

    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "webhooks" / "sample" =>
      withProject(req, pid, Role.Admin)((s, _) => projects.find(pid).flatMap {
        case None => err(Status.NotFound, "プロジェクトが見つかりません")
        case Some(p) =>
          val kind = param(req, "event").filter(EventKind.all.contains).getOrElse(EventKind.TaskUpdated)
          Ok(EventHub.sample(kind, p, (s.user.id, s.user.name)))
      })

    case req @ GET -> Root / "api" / "webhooks" / UuidVar(id) / "deliveries" =>
      withItem(req, integ.webhook(id), "Webhook")(_.projectId)((_, _) => integ.deliveries(id, 50).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "webhooks" / UuidVar(id) / "test" =>
      withItem(req, integ.webhook(id), "Webhook")(_.projectId)((s, w) =>
        webhooks.send(DeliveryRow(UUID.randomUUID(), w.id, w.url, w.secret, "ping",
          Json.obj("event" -> "ping".asJson, "at" -> Instant.now().asJson, "actor" -> s.user.name.asJson), 0)).flatMap { (r, code) =>
          Ok(Json.obj("ok" -> (r == OutboxResult.Sent).asJson, "statusCode" -> code.asJson, "result" -> r.toString.asJson))
        })

    // ---------------------------------------------------------------- 自動化
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "automations" =>
      withProject(req, pid, Role.Admin)((_, _) => integ.rules(pid).flatMap(_.traverse(ruleDto)).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "automations" =>
      withProject(req, pid, Role.Admin)((s, _) => body[RuleReq](req)(r => ruleOk(s, r) match
        case Left(e) => err(Status.BadRequest, e)
        case Right(_) =>
          val token = Option.when(r.trigger.hcursor.get[String]("type").contains("incoming_webhook"))(Crypto.randomToken(24))
          integ.createRule(pid, r.copy(name = r.name.trim), s.user.id, token).flatMap(id =>
            audit(s, "automation.create", pid, Json.obj("name" -> r.name.asJson)) *> integ.rule(id).flatMap(_.traverse(ruleDto)).flatMap(Created(_)))))

    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "automation-runs" =>
      withProject(req, pid, Role.Admin)((_, _) => integ.runs(pid, param(req, "ruleId").flatMap(uuidOpt), intParam(req, "limit", 50, 200)).flatMap(Ok(_)))

    case req @ PUT -> Root / "api" / "automations" / UuidVar(id) =>
      withItem(req, integ.rule(id), "ルール")(_.projectId)((s, old) => body[RuleReq](req)(r => ruleOk(s, r, old.runAs) match
        case Left(e) => err(Status.BadRequest, e)
        case Right(_) =>
          val token = Option.when(r.trigger.hcursor.get[String]("type").contains("incoming_webhook"))(Crypto.randomToken(24))
          integ.updateRule(id, r.copy(name = r.name.trim), token) *> audit(s, "automation.update", old.projectId) *> NoContent()))

    case req @ DELETE -> Root / "api" / "automations" / UuidVar(id) =>
      withItem(req, integ.rule(id), "ルール")(_.projectId)((s, r) => integ.deleteRule(id) *> audit(s, "automation.delete", r.projectId) *> NoContent())

    case req @ POST -> Root / "hooks" / "automation" / token =>
      req.bodyText.compile.string.flatMap { b =>
        val j = if b.trim.isEmpty then Json.obj() else io.circe.parser.parse(b).getOrElse(Json.fromString(b))
        respond(automation.incoming(token, j))(n => Accepted(Json.obj("queued" -> n.asJson)))
      }

    // ---------------------------------------------------------------- リポジトリ（SCM）
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "repos" =>
      withProject(req, pid, Role.View)((_, r) => integ.repos(pid).flatMap(rs =>
        Ok(rs.map(repoDto).map(d => if r >= Role.Admin then d else d.copy(secret = "")))))

    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "repos" =>
      withProject(req, pid, Role.Admin)((s, _) => body[RepoReq](req)(r => ScmProvider.parse(r.provider) match
        case None => err(Status.BadRequest, "プロバイダは github / gitlab / bitbucket / forgejo です")
        case Some(_) if (r.name.trim.isEmpty && !r.allRepos.contains(true)) || !httpUrl(r.webUrl) =>
          err(Status.BadRequest, "名前（owner/repo）と URL（http(s)://）を入力してください")
        case Some(_) if r.apiUrl.exists(u => u.trim.nonEmpty && !httpUrl(u)) => err(Status.BadRequest, "API の URL は http(s):// で始めてください")
        case Some(p) =>
          integ.createRepo(pid, r.copy(provider = p.asString, name = r.name.trim), Crypto.randomToken(24), r.token.filter(_.trim.nonEmpty).map(box.encrypt))
            .flatMap(id => audit(s, "repo.create", pid, Json.obj("name" -> r.name.asJson)) *> integ.repo(id).flatMap(x => Created(x.map(repoDto))))))

    case req @ PUT -> Root / "api" / "repos" / UuidVar(id) =>
      withItem(req, integ.repo(id), "リポジトリ")(_.projectId)((s, old) => body[RepoReq](req)(r => ScmProvider.parse(r.provider) match
        case None => err(Status.BadRequest, "プロバイダは github / gitlab / bitbucket / forgejo です")
        case Some(_) if !httpUrl(r.webUrl) || r.apiUrl.exists(u => u.trim.nonEmpty && !httpUrl(u)) =>
          err(Status.BadRequest, "URL は http(s):// で始めてください")
        case Some(p) =>
          // 送り先（プロバイダ・URL）を変えたら、保存してあるトークンは捨てる（別のサーバーへトークンを送らせない）
          val moved = old.provider != p.asString || old.webUrl != r.webUrl || old.apiUrl.filter(_.trim.nonEmpty) != r.apiUrl.filter(_.trim.nonEmpty)
          val newToken = r.token.filter(_.trim.nonEmpty)
          integ.updateRepo(id, r.copy(provider = p.asString), newToken.map(box.encrypt), r.token.contains("") || (moved && newToken.isEmpty)) *>
            audit(s, "repo.update", old.projectId) *> NoContent()))

    case req @ DELETE -> Root / "api" / "repos" / UuidVar(id) =>
      withItem(req, integ.repo(id), "リポジトリ")(_.projectId)((s, r) => integ.deleteRepo(id) *> audit(s, "repo.delete", r.projectId) *> NoContent())

    case req @ POST -> Root / "api" / "repos" / UuidVar(id) / "poll" =>
      withItem(req, integ.repo(id), "リポジトリ")(_.projectId)((_, r) => respond(scm.poll(r))(n => Ok(Json.obj("linked" -> n.asJson))))

    case req @ POST -> Root / "hooks" / "scm" / UuidVar(id) =>
      req.body.compile.to(Array).flatMap(b => respond(scm.receive(id, req.headers, b))(n => Ok(Json.obj("linked" -> n.asJson))))

  }
