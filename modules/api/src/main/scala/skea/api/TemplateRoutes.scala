package skea.api

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skea.core.*
import skea.db.*
import skea.shared.*

import java.time.{LocalDate, ZoneId}
import java.time.format.DateTimeFormatter
import java.util.{Locale, UUID}
import scala.concurrent.duration.*

/** テンプレートからのタスクの作成と、繰り返しタスク（決まった日に作る）。 */
final class TemplateService(templates: TemplateStore, projects: ProjectStore, svc: TaskService, sessions: SessionAuth, lock: JobLock,
    cfg: ApiConfig):

  /** テンプレートからタスク（とサブタスク）を作る。date は {{date}} と期日の起点。 */
  def instantiate(op: Op, tpl: TemplateDto, projectId: UUID, date: LocalDate, activeSprint: Boolean): Res[TaskDto] =
    val title = tpl.title.replace("{{date}}", date.toString).replace("{{month}}", date.toString.take(7))
    val sprint = if activeSprint then projects.sprints(projectId, includeClosed = false).map(_.find(_.state == "active").map(_.id)) else IO.pure(None)
    sprint.flatMap { sp =>
      val req = CreateTaskReq(title = title, description = tpl.description, statusId = tpl.statusId, sprintId = sp, assigneeId = tpl.assigneeId,
        dueDate = tpl.dueInDays.map(d => date.plusDays(d.toLong).toString), labelIds = tpl.labelIds, fields = tpl.fields, priority = tpl.priority,
        taskType = tpl.taskType)
      svc.create(op, projectId, req).flatMap {
        case Left(f) => IO.pure(Left(f))
        case Right(t) =>
          tpl.subtasks.traverse_(st => svc.create(op, projectId, CreateTaskReq(title = st, parentId = Some(t.id), sprintId = sp,
            assigneeId = tpl.assigneeId)).void).as(Right(t))
      }
    }

  def nextRun(frequency: String, weekday: Option[Int], monthday: Option[Int], after: LocalDate): LocalDate =
    Recurrence.next(frequency, weekday, monthday, after)

  def validate(r: RecurringReq): Either[String, Unit] =
    if !Frequency.all.contains(r.frequency) then Left("頻度は daily / weekly / monthly です")
    else if r.frequency == Frequency.Weekly && !r.weekday.exists(d => d >= 1 && d <= 7) then Left("毎週の曜日を選んでください")
    else if r.frequency == Frequency.Monthly && !r.monthday.exists(d => d >= 1 && d <= 31) then Left("毎月の日を選んでください")
    else Right(())

  def today: LocalDate = LocalDate.now(ZoneId.of(cfg.timezone))

  /** 作る日が来た繰り返しタスクを作る（作成者として動く。止まっていた間の分はまとめて1つだけ作る）。 */
  def runDue: IO[Unit] =
    val now = today
    templates.dueRecurring(now).flatMap(_.traverse_ { r =>
      val next = nextRun(r.frequency, r.weekday, r.monthday, now)
      (templates.template(r.templateId), r.createdBy.flatTraverse(u => sessions.sessionFor(u))).tupled.flatMap {
        case (Some((tpl, TemplateOwner.Project(_))), Some(s)) =>
          instantiate(Op(s), tpl, r.projectId, now, r.activeSprint).flatMap {
            case Right(t) => templates.markRun(r.id, r.nextRun, next, Some(t.key)).void
            case Left(f)  => skea.observe.Log.println(s"[skea] recurring task ${r.id} failed: ${f.message}") *> templates.markRun(r.id, r.nextRun, next, None).void
          }
        case _ => skea.observe.Log.println(s"[skea] recurring task ${r.id}: template or user not found") *> templates.markRun(r.id, r.nextRun, next, None).void
      }
    })

  /** 5分ごとに確認する（複数ノードでも1つだけが動く）。 */
  def loop: IO[Nothing] =
    Periodic.run(30.seconds, 5.minutes)(e => s"[skea] recurring tasks failed: $e")(lock.exclusive("recurring", 4.minutes)(runDue))

/** CSV・Jira からの取り込み（名前でステータス・担当者・ラベル・スプリントを探す。親子は ref でつなぐ）。 */
final class ImportService(projects: ProjectStore, users: UserStore, svc: TaskService, cfg: ApiConfig):

  private val jiraDate = List("d/MMM/yy h:mm a", "d/MMM/yy H:mm", "d/MMM/yy", "yyyy/MM/dd", "yyyy-MM-dd", "M/d/yyyy", "yyyy年M月d日")
    .map(p => DateTimeFormatter.ofPattern(p, Locale.ENGLISH))

  /** 日付（2026-11-15、2026/11/15、Jira の 15/Nov/26 10:00 AM など。時刻は捨てる）。 */
  def date(s: String): Option[LocalDate] =
    val t = s.trim
    if t.isEmpty then None
    else
      scala.util.Try(LocalDate.parse(t.take(10))).toOption.orElse(
        jiraDate.iterator.flatMap(f => scala.util.Try(LocalDate.parse(t, f)).toOption.orElse(
          scala.util.Try(java.time.LocalDateTime.parse(t, f).toLocalDate).toOption)).nextOption()).orElse(
        // 時刻つきの書式は、日付の部分だけで読む
        jiraDate.iterator.flatMap(f => scala.util.Try(LocalDate.parse(t.takeWhile(_ != ' '), f)).toOption).nextOption())

  /** ステータスの名前を、同じ名前か、名前から推した区分のステータスにする。 */
  private def statusFor(sts: List[StatusDto], name: String): (Option[UUID], Option[String]) =
    val n = name.trim.toLowerCase
    sts.find(_.name.toLowerCase == n) match
      case Some(st) => (Some(st.id), None)
      case None if n.isEmpty => (None, None)
      case None =>
        val cat =
          if "完了|終了|done|closed|resolved|fixed|クローズ".r.findFirstIn(n).isDefined then StatusCategory.Done
          else if "待|wait|block|hold|保留|pending".r.findFirstIn(n).isDefined then StatusCategory.Waiting
          else if "進行|対応中|作業中|progress|doing|review|レビュー|開発中".r.findFirstIn(n).isDefined then StatusCategory.InProgress
          else StatusCategory.Todo
        (sts.find(_.category == cat).map(_.id), Some(s"ステータス「$name」がないため「${sts.find(_.category == cat).map(_.name).getOrElse("")}」にしました"))

  def run(op: Op, projectId: UUID, req: ImportReq): Res[ImportResultDto] =
    if req.rows.size > 2000 then IO.pure(Left(Fail.Invalid("一度に取り込めるのは 2000 件までです")))
    else
      (projects.statuses(projectId), projects.sprints(projectId, includeClosed = false), projects.audience(projectId, cfg.adminGroups),
        users.list(None, includeDisabled = false, 5000)).tupled.flatMap { (sts, sprints, audience, all) =>
        val members = audience.map(_.id).toSet
        val warnings = List.newBuilder[String]
        def who(row: Int, v: Option[String]): Option[UUID] = v.map(_.trim).filter(_.nonEmpty).flatMap { name =>
          val n = name.toLowerCase
          all.filter(u => members(u.id)).find(u => u.name.toLowerCase == n || u.email.exists(_.toLowerCase == n)) match
            case Some(u) => Some(u.id)
            case None => warnings += s"${row}行目: 「$name」はプロジェクトのメンバーにいないため空にしました"; None
        }
        // 親を先に作る（ref が取り込みの中にある親子だけをつなぐ）
        val refs = req.rows.flatMap(_.ref).toSet
        val (parents, children) = req.rows.zipWithIndex.partition((r, _) => !r.parentRef.exists(refs))
        val prepared = (parents ++ children).map { (r, i) =>
          val row = i + 1
          val (st, sw) = r.status.fold((None: Option[UUID], None: Option[String]))(statusFor(sts, _))
          sw.foreach(w => warnings += s"${row}行目: $w")
          val sp = r.sprint.map(_.trim).filter(_.nonEmpty).flatMap(n => sprints.find(_.name == n).map(_.id).orElse {
            warnings += s"${row}行目: スプリント「$n」がないためバックログに入れました"; None
          })
          val start = r.startDate.flatMap(d => date(d).orElse { warnings += s"${row}行目: 開始日「$d」を読めません"; None })
          val due   = r.dueDate.flatMap(d => date(d).orElse { warnings += s"${row}行目: 期日「$d」を読めません"; None })
          r.priority.filter(p => Priority.parse(p).isEmpty).foreach(p => warnings += s"${row}行目: 優先度「$p」を読めないため「中」にしました")
          r.taskType.filter(t => TaskType.parse(t).isEmpty).foreach(t => warnings += s"${row}行目: 種類「$t」を読めないため「タスク」にしました")
          (row, r, st, sp, who(row, r.assignee), who(row, r.reporter), start, due.filter(d => start.forall(s => !d.isBefore(s))))
        }
        val bad = prepared.filter(_._2.title.trim.isEmpty).map(_._1)
        bad.foreach(r => warnings += s"${r}行目: タイトルがないため取り込みません")
        // 警告は行の順に並べる
        def sorted = warnings.result().sortBy(_.takeWhile(_.isDigit).toIntOption.getOrElse(0))
        if req.dryRun then IO.pure(Right(ImportResultDto(prepared.size - bad.size, Map.empty, sorted)))
        else
          prepared.filterNot(p => bad.contains(p._1)).foldLeftM((Map.empty[String, (UUID, String)], 0)) { case ((keys, n), (row, r, st, sp, as, rp, start, due)) =>
            for
              labels <- r.labels.map(_.trim).filter(_.nonEmpty).distinct.traverse(l => projects.ensureLabel(projectId, l, None).map(_.id))
              desc    = r.description.map(_.trim).filter(_.nonEmpty).map(RichText.fromMarkdown)
              parent  = r.parentRef.flatMap(keys.get).map(_._1)
              res    <- svc.create(op, projectId, CreateTaskReq(title = r.title.trim.take(500), description = desc, statusId = st, sprintId = sp,
                          assigneeId = as, reporterId = rp, parentId = parent, startDate = start.map(_.toString), dueDate = due.map(_.toString),
                          labelIds = labels, priority = r.priority.flatMap(Priority.parse), taskType = r.taskType.flatMap(TaskType.parse)), notify = false)
            yield res match
              case Right(t) => (r.ref.fold(keys)(ref => keys.updated(ref, (t.id, t.key))), n + 1)
              case Left(f)  => warnings += s"${row}行目: ${f.message}"; (keys, n)
          }.map((keys, n) => Right(ImportResultDto(n, keys.view.mapValues(_._2).toMap, sorted)))
      }

final class TemplateRoutes(templates: TemplateStore, svc: TemplateService, imports: ImportService, val sessions: SessionAuth, val access: Access2)
    extends RouteSupport:

  /** テンプレートを操作する（プロジェクトのものはプロジェクトの権限、個人のものは本人だけ）。 */
  private def withTemplate(req: Request[IO], id: UUID, need: Role)(f: (AuthSession, TemplateDto, TemplateOwner) => IO[Response[IO]]) =
    authed(req)(s => templates.template(id).flatMap {
      case Some((t, o @ TemplateOwner.Project(pid))) => respond(access.project(s, pid, need))(_ => f(s, t, o))
      case Some((t, o @ TemplateOwner.User(uid))) if uid == s.user.id => f(s, t, o)
      case _ => err(Status.NotFound, "テンプレートが見つかりません")
    })

  private def templateOk(r: TemplateReq): Option[String] =
    if r.name.trim.isEmpty || r.title.trim.isEmpty then Some("名前とタイトルを入力してください")
    else if r.dueInDays.exists(d => d < 0 || d > 3650) then Some("期日は 0〜3650 日後で指定してください")
    else if r.name.length > 100 || r.title.length > 500 then Some("名前は 100 文字、タイトルは 500 文字までです")
    else if r.subtasks.size > 50 || r.subtasks.exists(_.length > 500) then Some("サブタスクは 50 件（1件 500 文字）までです")
    else if r.description.exists(d => !RichText.valid(d)) then Some("説明の形式が正しくありません")
    else if r.priority.exists(Priority.parse(_).isEmpty) then Some("優先度が正しくありません")
    else if r.taskType.exists(TaskType.parse(_).isEmpty) then Some("種類が正しくありません")
    else None

  private def withRecurring(req: Request[IO], id: UUID)(f: (AuthSession, RecurringRow) => IO[Response[IO]]) =
    authed(req)(s => templates.recurringRow(id).flatMap {
      case None => err(Status.NotFound, "繰り返しタスクが見つかりません")
      case Some(r) => respond(access.project(s, r.projectId, Role.Edit))(_ => f(s, r))
    })

  /** 最初に作る日（startDate があればその日以降、なければ今日より後）。 */
  private def firstRun(r: RecurringReq): Either[String, LocalDate] =
    r.startDate.filter(_.nonEmpty) match
      case Some(d) => scala.util.Try(LocalDate.parse(d)).toEither.left.map(_ => "開始日は yyyy-MM-dd で指定してください")
          .map(d0 => svc.nextRun(r.frequency, r.weekday, r.monthday, d0.minusDays(1)))
      case None => Right(svc.nextRun(r.frequency, r.weekday, r.monthday, svc.today))

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // ---------------------------------------------------------------- テンプレート
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "templates" =>
      withProject(req, pid, Role.View)((_, _) => templates.templates(pid).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "templates" =>
      withProject(req, pid, Role.Edit)((s, _) => body[TemplateReq](req)(r => templateOk(r) match
        case Some(m) => err(Status.BadRequest, m)
        case None => templates.createTemplate(TemplateOwner.Project(pid), r, s.user.id).orFail.flatMap(_.fold(fail, id =>
            templates.template(id).flatMap(t => Created(t.map(_._1)))))))

    case req @ GET -> Root / "api" / "me" / "templates" =>
      authed(req)(s => templates.personalTemplates(s.user.id).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "me" / "templates" =>
      authed(req)(s => body[TemplateReq](req)(r => templateOk(r) match
        case Some(m) => err(Status.BadRequest, m)
        case None => templates.createTemplate(TemplateOwner.User(s.user.id), r, s.user.id).orFail.flatMap(_.fold(fail, id =>
            templates.template(id).flatMap(t => Created(t.map(_._1)))))))

    case req @ PUT -> Root / "api" / "templates" / UuidVar(id) =>
      withTemplate(req, id, Role.Edit)((_, _, o) => body[TemplateReq](req)(r => templateOk(r) match
        case Some(m) => err(Status.BadRequest, m)
        case None => templates.updateTemplate(id, o, r).orFail.flatMap(_.fold(fail, _ => NoContent()))))

    case req @ DELETE -> Root / "api" / "templates" / UuidVar(id) =>
      withTemplate(req, id, Role.Edit)((_, _, _) => templates.deleteTemplate(id) *> NoContent())

    /** テンプレートからタスクを作る（サブタスクも）。個人のテンプレートは projectId で作る先を指定する。 */
    case req @ POST -> Root / "api" / "templates" / UuidVar(id) / "tasks" =>
      withTemplate(req, id, Role.Edit)((s, t, o) =>
        val target = o match
          case TemplateOwner.Project(pid) => IO.pure(Right(pid))
          case TemplateOwner.User(_) => param(req, "projectId").flatMap(uuidOpt) match
              case Some(pid) => access.project(s, pid, Role.Edit).map(_.as(pid))
              case None      => IO.pure(Left(Fail.Invalid("作成先のプロジェクトを指定してください")))
        respond(target)(pid => respond(svc.instantiate(Op(s), t, pid, svc.today, param(req, "activeSprint").contains("true")))(Created(_))))

    // ---------------------------------------------------------------- 繰り返しタスク
    case req @ GET -> Root / "api" / "projects" / UuidVar(pid) / "recurring" =>
      withProject(req, pid, Role.View)((_, _) => templates.recurring(pid).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "recurring" =>
      withProject(req, pid, Role.Edit)((s, _) => body[RecurringReq](req)(r =>
        (svc.validate(r), firstRun(r)).tupled match
          case Left(m) => err(Status.BadRequest, m)
          case Right((_, first)) => templates.template(r.templateId).flatMap {
              case Some((_, TemplateOwner.Project(tp))) if tp == pid => templates.createRecurring(pid, r, first, s.user.id) *> templates.recurring(pid).flatMap(Created(_))
              case _ => err(Status.BadRequest, "テンプレートが見つかりません")
            }))

    case req @ PUT -> Root / "api" / "recurring" / UuidVar(id) =>
      withRecurring(req, id)((_, old) => body[RecurringReq](req)(r =>
        (svc.validate(r), firstRun(r)).tupled match
          case Left(m) => err(Status.BadRequest, m)
          case Right((_, first)) =>
            // 頻度・曜日・日を変えなければ、次に作る日はそのまま
            val same = old.frequency == r.frequency && old.weekday == r.weekday && old.monthday == r.monthday && r.startDate.forall(_.isEmpty)
            // テンプレートは同じプロジェクトのものだけ（ほかのプロジェクトのテンプレートを使わせない）
            templates.template(r.templateId).flatMap {
              case Some((_, TemplateOwner.Project(tp))) if tp == old.projectId =>
                templates.updateRecurring(id, r, if same then old.nextRun else first) *> NoContent()
              case _ => err(Status.BadRequest, "テンプレートが見つかりません")
            }))

    case req @ DELETE -> Root / "api" / "recurring" / UuidVar(id) =>
      withRecurring(req, id)((_, _) => templates.deleteRecurring(id) *> NoContent())

    // ---------------------------------------------------------------- 取り込み
    case req @ POST -> Root / "api" / "projects" / UuidVar(pid) / "import" =>
      withProject(req, pid, Role.Edit)((s, _) => body[ImportReq](req)(r => respond(imports.run(Op(s), pid, r))(Ok(_))))
  }
