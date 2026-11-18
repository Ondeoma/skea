package skea.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skea.core.*
import skea.db.*
import skea.observe.{Metrics, Telemetry}
import skea.shared.*

import java.util.UUID

/**
 * ヘルプデスク。
 *
 *  - 窓口（/api/help/…）: 問い合わせする"ユーザ"の API。自分に見えるタスク（自分が報告者か、自分・自分のグループに共有されたもの）だけを扱い、
 *    内部コメント・作業時間・ラベルなどエージェント向けのものは返さない。エージェントも同じ画面で確かめられる。
 *  - 設定（/api/projects/{id}/helpdesk・requester-groups）と、タスクの共有（/api/tasks/{id}/shares）: エージェントの API。
 */
final class HelpRoutes(
    help: HelpStore,
    projects: ProjectStore,
    tasks: TaskStore,
    activity: ActivityStore,
    svc: TaskService,
    hub: EventHub,
    notifier: NotifyService,
    files: Attachments,
    val sessions: SessionAuth,
    val access: Access2,
    cfg: ApiConfig
) extends RouteSupport:

  private def audit(s: AuthSession, action: String, target: UUID, detail: Json = Json.obj()) =
    activity.audit(Some(s.user.id), s.viaString, action, Some(target.toString), detail)

  private def portalDto(p: HelpdeskProject, role: Option[Role]) =
    HelpPortalDto(p.id, p.key, p.name, p.description, p.acceptsGuests, role.map(_.asString))

  /** 窓口で見せる添付ファイルの URL（エージェントの /api/attachments は"ユーザ"には使えない）。 */
  private def helpUrl(a: AttachmentDto): AttachmentDto = a.copy(url = s"/api/help/attachments/${a.id}")

  /** 本文に埋め込んだ画像・ファイルのリンクも窓口の URL にする（そのタスクの添付だけが読める）。 */
  private def helpHtml(html: String): String = html.replace("\"/api/attachments/", "\"/api/help/attachments/")

  /** ヘルプデスクのプロジェクトに参加していれば（"ユーザ"でもエージェントでも）処理する。 */
  private def withHelpProject(req: Request[IO], pid: UUID)(f: (AuthSession, HelpdeskProject, Role) => IO[Response[IO]]): IO[Response[IO]] =
    authed(req)(s => help.project(pid).flatMap {
      case Some(p) if p.helpdesk && !p.archived => respond(access.project(s, pid, Role.Requester))(r => f(s, p, r))
      case _                                    => err(Status.NotFound, "窓口が見つかりません")
    })

  /** 窓口でタスクを扱えるか: エージェントはすべて、"ユーザ"は自分に見えるものだけ（見えなければ 404）。 */
  private def helpTask(s: AuthSession, id: String): Res[(TaskCore, Role)] =
    access.task(s, id, Role.Requester).flatMap {
      case Left(f) => IO.pure(Left(f))
      case Right((t, r)) =>
        help.project(t.projectId).flatMap {
          case Some(p) if p.helpdesk && !p.archived =>
            (if r.isAgent then IO.pure(true) else help.canSee(t.id, s.user.id))
              .map(ok => Either.cond(ok, (t, r), Fail.NotFound("タスクが見つかりません")))
          case _ => IO.pure(Left(Fail.NotFound("タスクが見つかりません")))
        }
    }

  private def withHelpTask(req: Request[IO], id: String)(f: (AuthSession, TaskCore, Role) => IO[Response[IO]]): IO[Response[IO]] =
    authed(req)(s => respond(helpTask(s, id))((t, r) => f(s, t, r)))

  private def detail(s: AuthSession, t: TaskCore, r: Role): IO[Response[IO]] =
    (help.task(t.id, s.user.id), help.project(t.projectId), tasks.comments(t.id, withInternal = false), tasks.attachments(t.id),
      help.shareNames(t.id), tasks.watchers(t.id), help.formValues(t.id)).tupled.flatMap {
      case (Some(ht), Some(p), cs, as, names, ws, fs) =>
        activity.markTaskRead(s.user.id, t.id).whenA(s.isSession) *>
          Ok(HelpTaskDetailDto(ht, p.key, p.name, t.description, helpHtml(t.description.fold("")(RichText.toHtml)),
            cs.map(c => c.copy(html = helpHtml(c.html))), as.map(helpUrl), names,
            ws.contains(s.user.id), r.isAgent, fs))
      case _ => err(Status.NotFound, "タスクが見つかりません")
    }

  /** 新しい問い合わせを、プロジェクトの編集者・管理者（回答するエージェント）に知らせる。 */
  private def notifyAgents(op: Op, t: TaskDto): IO[Unit] =
    projects.usersWithRole(t.projectId, "edit").flatMap(ids =>
      notifier.send(NotificationKind.Request, ids.toSet, Some(op.userId), t.projectId, Some(t.id), None,
        s"${t.key} 新しい問い合わせ", t.title, s"/tasks/${t.key}"))

  /**
   * 窓口での共有先: 今の共有（自分が共有したものは外せる）と、共有できるグループ（自分のメールアドレスと同じドメインのもの）。
   * 共有できるのは、自分が問い合わせたタスクだけ。
   */
  private def helpShares(s: AuthSession, t: TaskCore): IO[HelpSharesDto] =
    (help.groups(t.projectId), help.groupShares(t.id)).tupled.map { (gs, cur) =>
      val mine    = t.reporterId.contains(s.user.id)
      val allowed = if mine then RequesterGroups.shareable(gs, s.user.email) else Nil
      val curIds  = cur.map(_._1).toSet
      HelpSharesDto(
        cur.flatMap((gid, by) => gs.find(_.id == gid).map(g => HelpShareItemDto(gid, RequesterGroups.path(gs, g), mine && by.contains(s.user.id)))),
        allowed.filterNot(g => curIds(g.id)).map(g => HelpShareItemDto(g.id, RequesterGroups.path(gs, g), removable = true))
          .sortBy(_.name),
        mine && allowed.nonEmpty)
    }

  /** このメールアドレスの"ユーザ"が共有できるグループ（id と階層の名前）。 */
  private def shareOptions(s: AuthSession, projectId: UUID): IO[List[HelpShareItemDto]] =
    help.groups(projectId).map(gs => RequesterGroups.shareable(gs, s.user.email)
      .map(g => HelpShareItemDto(g.id, RequesterGroups.path(gs, g), removable = true)).sortBy(_.name))

  /** 窓口のフォームに出す項目（カスタムフィールドのうち helpForm のもの）。 */
  private def formFields(projectId: UUID): IO[List[CustomFieldDto]] = projects.fields(projectId).map(_.filter(_.helpForm))

  /** フォームの入力を確かめる（フォームにない項目は受け付けず、必須の項目は空にできない）。値の形はタスクの作成で確かめる。 */
  private def formInput(form: List[CustomFieldDto], values: Map[String, Json]): Either[Fail, Map[String, Json]] =
    def blank(j: Json) = j.isNull || j.asString.exists(_.trim.isEmpty) || j.asArray.exists(_.isEmpty)
    values.keys.find(k => !form.exists(_.id.toString == k)) match
      case Some(_) => Left(Fail.Invalid("窓口のフォームにない項目は入力できません"))
      case None =>
        form.find(f => f.required && values.get(f.id.toString).forall(blank)) match
          case Some(f) => Left(Fail.Invalid(s"「${f.name}」を入力してください"))
          case None    => Right(values.filterNot((_, v) => blank(v)))

  /** 内容に貼った一時置き場のファイルの URL（/api/help/uploads/ID）。 */
  private val PendingUrl = "/api/help/uploads/([0-9a-fA-F-]{36})".r

  /** 内容から一時置き場のファイルの ID を取り出す。 */
  private def pendingRefs(desc: Option[Json]): List[UUID] =
    desc.toList.flatMap(d => PendingUrl.findAllMatchIn(d.noSpaces).flatMap(m => uuidOpt(m.group(1)))).distinct

  /** 内容の一時置き場の URL を、添付の URL（id はそのまま）にする。 */
  private def toAttachmentUrls(desc: Json, ids: List[UUID]): Json =
    if ids.isEmpty then desc
    else io.circe.parser.parse(ids.foldLeft(desc.noSpaces)((t, id) => t.replace(s"/api/help/uploads/$id", s"/api/attachments/$id"))).getOrElse(desc)

  private val notSameDomain = Fail.Forbidden("共有できるのは、自分のメールアドレスと同じドメインのグループだけです")

  /**
   * "ユーザ"が自分の問い合わせの共有先を want（自分が共有したいグループの全体）にする。
   * ほかの人（エージェント）が共有したものはそのまま残し、外せるのは自分が共有したものだけ。共有したグループの人はウォッチャーにも登録し、通知する。
   */
  private def requesterShare(s: AuthSession, t: TaskCore, want0: List[UUID]): Res[Unit] =
    if !t.reporterId.contains(s.user.id) then IO.pure(Left(Fail.Forbidden("共有できるのは自分の問い合わせだけです")))
    else
      (help.groups(t.projectId), help.groupShares(t.id), help.shares(t.id, t.projectId)).tupled.flatMap { (gs, cur, all) =>
        val allowed = RequesterGroups.shareable(gs, s.user.email).map(_.id).toSet
        val want    = want0.distinct
        val kept    = cur.filter((gid, by) => !by.contains(s.user.id) || want.contains(gid)).map(_._1)
        val adding  = want.filterNot(kept.contains)
        if adding.exists(g => !allowed(g)) then IO.pure(Left(notSameDomain))
        else
          val op = Op(s)
          Telemetry.span("helpdesk.share", "task.id" -> t.id.toString, "by" -> "requester")(
            help.setShares(t.id, t.projectId, (kept ++ adding).distinct, all.users.map(_.id), s.user.id)).orFail.flatMap {
            case Left(f) => IO.pure(Left(f))
            case Right(added) =>
              (hub.touched(op, t, "shares") *> Metrics.count("skea.helpdesk.shares", "by" -> "requester") *>
                notifier.send(NotificationKind.Shared, added, Some(op.userId), t.projectId, Some(t.id), None,
                  s"${t.key} が共有されました", t.title, s"/tasks/${t.key}")).as(Right(()))
          }
      }

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // ---------------------------------------------------------------- 窓口（"ユーザ"）

    /** 参加している窓口の一覧。 */
    case req @ GET -> Root / "api" / "help" / "projects" =>
      authed(req)(s => projects.memberships(s).flatMap(ms =>
        help.helpdeskProjects(ms.keys.toList).flatMap(ps => Ok(ps.map(p => portalDto(p, ms.get(p.id)))))))

    /** 窓口の案内（ログインしていなくても、ゲスト登録を許可していれば見られる）。 */
    case req @ GET -> Root / "api" / "help" / "portal" / key =>
      (help.projectByKey(key), sessions.fromRequest(req)).tupled.flatMap {
        case (Some(p), s) if p.helpdesk && !p.archived =>
          s.flatTraverse(x => projects.role(p.id, x)).flatMap {
            case Some(r)                  => Ok(portalDto(p, Some(r)))
            case None if p.acceptsGuests => Ok(portalDto(p, None))
            case None                     => err(Status.NotFound, "窓口が見つかりません")
          }
        case _ => err(Status.NotFound, "窓口が見つかりません")
      }

    /** ゲスト登録: OIDC でログインした人が、窓口から"ユーザ"として参加する。 */
    case req @ POST -> Root / "api" / "help" / "portal" / key / "join" =>
      sessionOnly(req)(s => help.projectByKey(key).flatMap {
        case Some(p) if p.acceptsGuests =>
          projects.role(p.id, s).flatMap {
            case Some(r) => Ok(portalDto(p, Some(r)))
            case None if !p.domainAllowed(s.user.email) =>
              err(Status.Forbidden, s"この窓口に登録できるのは ${p.guestDomains.map("@" + _).mkString("・")} のメールアドレスの人だけです")
            case None =>
              Telemetry.span("helpdesk.signup", "project.id" -> p.id.toString) {
                help.join(p.id, s.user.id).flatTap(added => IO.whenA(added)(
                  audit(s, "helpdesk.signup", p.id, Json.obj("key" -> p.key.asJson)) *> Metrics.count("skea.helpdesk.signups", "project" -> p.key)))
              } *> Ok(portalDto(p, Some(Role.Requester)))
          }
        case _ => err(Status.NotFound, "この窓口にはゲスト登録できません")
      })

    /** 自分に見える問い合わせ（done=true なら完了したものも）。 */
    case req @ GET -> Root / "api" / "help" / "projects" / UuidVar(pid) / "tasks" =>
      withHelpProject(req, pid)((s, _, _) => help.visibleTasks(pid, s.user.id, param(req, "done").contains("true")).flatMap(Ok(_)))

    /**
     * 窓口の検索（絞り込み構文 + 全文検索。例: `VPN status:進行中 created>-30d order:-updated`）。
     * 対象は自分に見えるタスクだけで、"ユーザ"に見せない項目（ラベル・優先度・内部コメントなど）では絞り込めない。
     */
    case req @ GET -> Root / "api" / "help" / "projects" / UuidVar(pid) / "search" =>
      withHelpProject(req, pid)((s, _, r) => SearchQuery.parse(param(req, "q").getOrElse("")) match
        case Left(e) => Ok(HelpSearchResultDto(Nil, 0, Some(e)))
        case Right(q) =>
          Telemetry.span("helpdesk.search", "project.id" -> pid.toString) {
            help.search(pid, s.user.id, r.isAgent, q, Op(s).today, intParam(req, "limit", 50, 200),
              param(req, "offset").flatMap(_.toIntOption).map(math.max(0, _)).getOrElse(0))
          }.flatMap { res =>
            Metrics.count("skea.helpdesk.searches", "ok" -> res.isRight.toString) *>
              Ok(res.fold(e => HelpSearchResultDto(Nil, 0, Some(e)), identity))
          })

    /** ステータスの一覧（検索の条件を選ぶのに使う）。 */
    case req @ GET -> Root / "api" / "help" / "projects" / UuidVar(pid) / "statuses" =>
      withHelpProject(req, pid)((_, _, _) => projects.statuses(pid).flatMap(Ok(_)))

    /** 新しい問い合わせのフォームの項目（プロジェクトの設定で選んだカスタムフィールド）。 */
    case req @ GET -> Root / "api" / "help" / "projects" / UuidVar(pid) / "form" =>
      withHelpProject(req, pid)((_, _, _) => formFields(pid).flatMap(Ok(_)))

    /**
     * 新しい問い合わせの内容に貼る画像・ファイルを、送信する前に一時置き場に置く（本人だけが見られる）。
     * 送信したときに内容に残っているものは問い合わせの添付になる。1日たって使われなかったものは、次に置いたときに消す。
     */
    case req @ POST -> Root / "api" / "help" / "projects" / UuidVar(pid) / "uploads" =>
      withHelpProject(req, pid)((s, _, _) =>
        if !files.available then err(Status.ServiceUnavailable, "添付ファイルの保存先（S3）が設定されていません")
        else withUpload(req, cfg.maxUploadBytes) { up =>
          val id = UUID.randomUUID()
          val key = s"uploads/$pid/$id"
          for
            old <- help.purgePending(s.user.id, java.time.Instant.now().minusSeconds(86400))
            _   <- files.remove(old)
            sha <- files.put(key, up)
            res <- sha.fold(NotFound()) { h =>
                     help.addPending(id, s.user.id, pid, key, up.name, up.size, up.contentType, h) *>
                       Metrics.count("skea.helpdesk.uploads") *> files.dto(s.user.id, id, up, s"/api/help/uploads/$id").flatMap(Created(_))
                   }
          yield res
        })

    /** 一時置き場のファイル（置いた本人だけ）。 */
    case req @ GET -> Root / "api" / "help" / "uploads" / UuidVar(id) =>
      authed(req)(s => help.pending(id, s.user.id).flatMap {
        case Some((key, name, ct)) => files.download(key, name, ct)
        case None                  => err(Status.NotFound, "ファイルが見つかりません")
      })

    /** 自分が共有できるグループ（新しい問い合わせで共有先を選ぶ）。 */
    case req @ GET -> Root / "api" / "help" / "projects" / UuidVar(pid) / "share-options" =>
      withHelpProject(req, pid)((s, _, _) => shareOptions(s, pid).flatMap(Ok(_)))

    /** 問い合わせる（タイトルと説明。報告者は自分）。groupIds があれば、作成と同時にそのグループに共有する（同じドメインのものだけ）。 */
    case req @ POST -> Root / "api" / "help" / "projects" / UuidVar(pid) / "tasks" =>
      withHelpProject(req, pid)((s, _, _) => body[HelpRequestReq](req) { r =>
        val op     = Op(s)
        val groups = r.groupIds.getOrElse(Nil).distinct
        // 共有先とフォームの入力を先に確かめる（だめなら作らない）
        (shareOptions(s, pid), formFields(pid)).tupled.flatMap { (os, form) =>
          (if groups.forall(g => os.exists(_.groupId == g)) then formInput(form, r.fields.getOrElse(Map.empty)) else Left(notSameDomain)) match
          case Left(f) => fail(f)
          case Right(values) =>
            Telemetry.span("helpdesk.request.create", "project.id" -> pid.toString) {
              // 説明は Tiptap の文書か、文字列（Markdown）。貼った画像（一時置き場）は添付の URL にする
              val desc0 = r.description.map(d => d.asString.fold(d)(RichText.fromMarkdown))
              help.pendingIds(pendingRefs(desc0), s.user.id, pid).flatMap { pend =>
                val desc = desc0.map(toAttachmentUrls(_, pend))
                svc.create(op, pid, CreateTaskReq(r.title.trim.take(300), desc, fields = values), need = Role.Requester)
                  .flatTap(_.traverse_(t => IO.whenA(pend.nonEmpty)(help.attachPending(pend, s.user.id, t.id).void)))
              }
            }.flatMap {
              case Left(f) => fail(f)
              case Right(t) =>
                val share = if groups.isEmpty then IO.pure(Right(()))
                            else tasks.core(t.id).flatMap(_.fold(IO.pure(Right(()): Either[Fail, Unit]))(c => requesterShare(s, c, groups)))
                share *> notifyAgents(op, t) *> Metrics.count("skea.helpdesk.requests.created", "via" -> op.via, "shared" -> groups.nonEmpty.toString) *>
                  help.task(t.id, s.user.id).flatMap(_.fold(NotFound())(Created(_)))
            }
        }
      })

    case req @ GET -> Root / "api" / "help" / "tasks" / id =>
      withHelpTask(req, id)(detail)

    /** 返信する（"ユーザ"のコメントは常に公開。内部コメントはエージェントの画面から）。 */
    case req @ POST -> Root / "api" / "help" / "tasks" / id / "comments" =>
      withHelpTask(req, id)((s, t, r) => body[CommentReq](req)(c =>
        respond(svc.addComment(Op(s), t, c.body, internal = false))(dto =>
          Metrics.count("skea.helpdesk.replies", "from" -> (if r.isAgent then "agent" else "requester")) *> Created(dto.copy(html = helpHtml(dto.html))))))

    case req @ POST -> Root / "api" / "help" / "tasks" / id / "attachments" =>
      withHelpTask(req, id)((s, t, _) =>
        if !files.available then err(Status.ServiceUnavailable, "添付ファイルの保存先（S3）が設定されていません")
        else withUpload(req, cfg.maxUploadBytes)(up => files.save(Op(s), t, up).flatMap(_.fold(NotFound())(a => Created(helpUrl(a))))))

    case req @ GET -> Root / "api" / "help" / "attachments" / UuidVar(aid) =>
      authed(req)(s => tasks.attachment(aid).flatMap {
        case None => err(Status.NotFound, "添付ファイルが見つかりません")
        case Some((tid, key, name, ct, _)) => respond(helpTask(s, tid.toString))(_ => files.download(key, name, ct))
      })

    case req @ GET -> Root / "api" / "help" / "tasks" / id / "shares" =>
      withHelpTask(req, id)((s, t, _) => helpShares(s, t).flatMap(Ok(_)))

    /**
     * 自分の問い合わせを、自分のメールアドレスと同じドメインのグループに共有する（groupIds は自分が共有したいグループの全体）。
     * ほかの人（エージェント）が共有したものはそのまま残し、外せるのは自分が共有したものだけ。共有したグループの人はウォッチャーにも登録する。
     */
    case req @ PUT -> Root / "api" / "help" / "tasks" / id / "shares" =>
      withHelpTask(req, id)((s, t, _) => body[HelpSharesReq](req)(r =>
        respond(requesterShare(s, t, r.groupIds))(_ => helpShares(s, t).flatMap(Ok(_)))))

    /** 更新を通知で受け取る・やめる。 */
    case req @ POST -> Root / "api" / "help" / "tasks" / id / "watch" =>
      withHelpTask(req, id)((s, t, _) => tasks.watch(t.id, s.user.id) *> NoContent())

    case req @ DELETE -> Root / "api" / "help" / "tasks" / id / "watch" =>
      withHelpTask(req, id)((s, t, _) => tasks.unwatch(t.id, s.user.id) *> NoContent())

    // ---------------------------------------------------------------- 設定（エージェント）

    /** 設定・グループ・"ユーザ"の一覧（共有先を選ぶのに使うので閲覧権限で見られる）。 */
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "helpdesk" =>
      withProject(req, pid, Role.View)((_, _) => help.settings(pid).flatMap(_.fold(NotFound())(Ok(_))))

    case req @ PUT -> Root / "api" / "projects" / UuidVar(pid) / "helpdesk" =>
      withProject(req, pid, Role.Admin)((s, _) => body[HelpdeskSettingsReq](req)(r =>
        if r.guestDomains.exists(d => !d.trim.stripPrefix("@").matches("^[A-Za-z0-9.-]*$")) then err(Status.BadRequest, "ドメインは example.com の形で入力してください")
        else if r.doneHideDays.exists(d => d < 0 || d > 3650) then err(Status.BadRequest, "ボードに完了したタスクを出す日数は 0〜3650 日で指定してください")
        // 画面はこの知らせでプロジェクトの設定（スプリント）を読み直す
        else help.updateSettings(pid, r) *> audit(s, "project.helpdesk", pid, r.asJson) *>
          hub.emit(Op(s), pid, None, "project.helpdesk", Json.obj("helpdesk" -> r.helpdesk.asJson)) *> NoContent()))

    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "requester-groups" =>
      withProject(req, pid, Role.Admin)((s, _) => body[RequesterGroupReq](req)(r =>
        if r.name.trim.isEmpty || r.name.length > 100 then err(Status.BadRequest, "グループの名前を 100 文字までで入力してください")
        else help.createGroup(pid, r).orFail.flatMap(_.fold(fail, id =>
          audit(s, "helpdesk.group.create", pid, Json.obj("name" -> r.name.asJson)) *> Created(Json.obj("id" -> id.asJson))))))

    case req @ PUT -> Root / "api" / "projects" / UuidVar(pid) / "requester-groups" / UuidVar(gid) =>
      withProject(req, pid, Role.Admin)((s, _) => body[RequesterGroupReq](req)(r =>
        if r.name.trim.isEmpty || r.name.length > 100 then err(Status.BadRequest, "グループの名前を 100 文字までで入力してください")
        else help.updateGroup(pid, gid, r).orFail.flatMap(_.fold(fail, _ => audit(s, "helpdesk.group.update", pid) *> NoContent()))))

    case req @ DELETE -> Root / "api" / "projects" / UuidVar(pid) / "requester-groups" / UuidVar(gid) =>
      withProject(req, pid, Role.Admin)((s, _) => help.deleteGroup(pid, gid) *> audit(s, "helpdesk.group.delete", pid) *> NoContent())

    case req @ PUT -> Root / "api" / "projects" / UuidVar(pid) / "requester-groups" / UuidVar(gid) / "members" =>
      withProject(req, pid, Role.Admin)((_, _) => body[GroupMemberReq](req)(r =>
        help.addGroupMember(pid, gid, r.userId).orFail.flatMap(_.fold(fail, _ => NoContent()))))

    case req @ DELETE -> Root / "api" / "projects" / UuidVar(pid) / "requester-groups" / UuidVar(gid) / "members" / UuidVar(uid) =>
      withProject(req, pid, Role.Admin)((_, _) => help.removeGroupMember(pid, gid, uid) *> NoContent())

    // ---------------------------------------------------------------- タスクの共有（エージェント）

    case req @ GET -> Root / "api" / "tasks" / id / "shares" =>
      withTask(req, id, Role.View)((_, t, _) => help.shares(t.id, t.projectId).flatMap(Ok(_)))

    /** 共有先を置き換える。notifyNew なら、新しく見えるようになった"ユーザ"に知らせる。 */
    case req @ PUT -> Root / "api" / "tasks" / id / "shares" =>
      withTask(req, id, Role.Edit)((s, t, _) => body[TaskSharesReq](req) { r =>
        val op = Op(s)
        Telemetry.span("helpdesk.share", "task.id" -> t.id.toString)(help.setShares(t.id, t.projectId, r.groupIds, r.userIds, s.user.id, r.watch.getOrElse(true))).orFail.flatMap {
          case Left(f) => fail(f)
          case Right(added) =>
            for
              _ <- hub.touched(op, t, "shares")
              _ <- Metrics.count("skea.helpdesk.shares", "notify" -> r.notifyNew.toString)
              _ <- IO.whenA(r.notifyNew)(notifier.send(NotificationKind.Shared, added, Some(op.userId), t.projectId, Some(t.id), None,
                     s"${t.key} が共有されました", t.title, s"/tasks/${t.key}"))
              res <- help.shares(t.id, t.projectId).flatMap(Ok(_))
            yield res
        }
      })
  }
