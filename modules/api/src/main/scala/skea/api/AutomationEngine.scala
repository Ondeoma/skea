package skea.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.headers.`Content-Type`
import skea.core.*
import skea.core.Automation.*
import skea.db.*
import skea.shared.*

import java.time.{Instant, LocalDate}
import java.util.UUID
import scala.concurrent.duration.*

/** 自動化のルールの実行。イベント（EventHub が automation_queue に積んだもの）・定期・手動・受信 Webhook から起動する。 */
final class AutomationEngine(
    integ: IntegrationStore,
    tasks: TaskStore,
    projects: ProjectStore,
    users: UserStore,
    activity: ActivityStore,
    search: SearchStore,
    sessions: SessionAuth,
    access: Access2,
    svc: TaskService,
    notifier: NotifyService,
    http: Client[IO],
    lock: JobLock,
    cfg: ApiConfig
):

  private val zone = Op.zone(cfg.timezone)

  /** 1回の実行で動かすアクションの上限（無限ループ・暴走の防止）。 */
  private val MaxSteps = 200

  // ---------------------------------------------------------------- 起動

  def loop: IO[Nothing] =
    Periodic.run(5.seconds, 3.seconds)(e => s"[skea] automation worker failed: $e")(integ.processQueue(20)(runQueued))

  def purgeLoop: IO[Nothing] =
    Periodic.run(2.hours, 24.hours)(e => s"[skea] automation purge failed: $e")(integ.purgeQueue(7))

  /** 定期トリガー（1分ごとに確認。複数ノードでも1つだけが動く）。 */
  def scheduledLoop: IO[Nothing] =
    Periodic.run(30.seconds, 1.minute)(e => s"[skea] scheduled automation failed: $e")(
      lock.exclusive("automation-scheduled", 50.seconds) {
        IO.realTimeInstant.flatMap { now =>
          integ.scheduledRules.flatMap(_.traverse_ { r =>
            parseTrigger(r.trigger).toOption match
              case Some((t: Trigger.Scheduled, _)) if scheduledDue(t, r.lastRunAt.getOrElse(r.createdAt), now, zone) =>
                integ.touchRule(r.id, now) *> fireFor(r, t.query, Json.obj("event" -> "scheduled".asJson, "at" -> now.asJson))
              case _ => IO.unit
          })
        }
      }
    )

  /** query に一致するタスクごとに（query がなければタスクなしで1回）実行待ちに積む。 */
  private def fireFor(r: RuleRow, query: Option[String], event: Json): IO[Int] =
    query match
      case None => integ.enqueueRun(r.id, None, None, event, 0).as(1)
      case Some(q) =>
        runAs(r).flatMap {
          case None => IO.pure(0)
          case Some(s) =>
            SearchQuery.parse(q) match
              case Left(_) => IO.pure(0)
              case Right(parsed) =>
                search.search(parsed, search.Ctx(s.user.id, List(r.projectId), LocalDate.now(zone)), 200, 0).flatMap {
                  case Left(_)    => IO.pure(0)
                  case Right(res) => res.hits.traverse_(h => integ.enqueueRun(r.id, Some(h.task.id), None, event, 0)).as(res.hits.size)
                }
        }

  /** タスク画面のボタンから。 */
  def manual(op: Op, ruleId: UUID, task: TaskCore): Res[Unit] =
    integ.rule(ruleId).flatMap {
      case Some(r) if r.projectId == task.projectId && r.enabled && r.trigger.hcursor.get[String]("type").contains("manual") =>
        integ.enqueueRun(r.id, Some(task.id), Some(op.userId), Json.obj("event" -> "manual".asJson, "actor" -> op.s.user.name.asJson), op.depth).as(Right(()))
      case _ => IO.pure(Left(Fail.NotFound("ルールが見つかりません")))
    }

  /** 受信 Webhook（POST /hooks/automation/{token}）。本文は {{webhookData}} で参照できる。taskKey を送るとそのタスクで実行する。 */
  def incoming(token: String, body: Json): Res[Int] =
    integ.ruleByToken(token).flatMap {
      case None => IO.pure(Left(Fail.NotFound("ルールが見つかりません")))
      case Some(r) =>
        val event = Json.obj("event" -> "incoming_webhook".asJson, "webhookData" -> body)
        body.hcursor.get[String]("taskKey").toOption match
          case Some(k) =>
            tasks.coreByKey(k).flatMap {
              case Some(t) if t.projectId == r.projectId => integ.enqueueRun(r.id, Some(t.id), None, event, 0).as(Right(1))
              case _                                     => IO.pure(Left(Fail.NotFound("タスクが見つかりません")))
            }
          case None =>
            val q = parseTrigger(r.trigger).toOption.collect { case (Trigger.IncomingWebhook(q), _) => q }.flatten
            fireFor(r, q, event).map(Right(_))
    }

  private def runAs(r: RuleRow): IO[Option[AuthSession]] =
    r.runAs.orElse(r.createdBy).flatTraverse(u => sessions.sessionFor(u, AuthVia.Automation(r.id)))

  // ---------------------------------------------------------------- 実行

  /** 実行中の状態。 */
  private final class Run(val rule: RuleRow, val op: Op, var ctx: Json, var log: Vector[Json], var steps: Int):
    def add(level: String, msg: String): Unit = log = log :+ Json.obj("level" -> level.asJson, "message" -> msg.asJson)
    def today: LocalDate = LocalDate.now(zone)
    def render(s: String): String = Automation.render(s, ctx, today)
    def taskId: Option[UUID] = ctx.hcursor.downField("task").get[UUID]("id").toOption
    def setVar(name: String, v: Json): Unit =
      val vars = ctx.hcursor.downField("vars").focus.flatMap(_.asObject).getOrElse(io.circe.JsonObject.empty).add(name, v)
      ctx = ctx.deepMerge(Json.obj("vars" -> Json.fromJsonObject(vars), name -> v))

  private def runQueued(q: QueueRow): IO[Unit] =
    integ.rule(q.ruleId).flatMap {
      case None                     => IO.unit
      case Some(r) if !r.enabled    => IO.unit
      case Some(r) =>
        (parseTrigger(r.trigger), parseActions(r.actions)).tupled match
          case Left(e) => integ.saveRun(r.id, q.taskId, "error", Json.arr(Json.obj("level" -> "error".asJson, "message" -> e.asJson)), Instant.now())
          case Right(((trigger, cond), actions)) =>
            runAs(r).flatMap {
              case None => integ.saveRun(r.id, q.taskId, "error", Json.arr(Json.obj("level" -> "error".asJson,
                             "message" -> "実行するユーザーが無効です".asJson)), Instant.now())
              case Some(s) =>
                access.project(s, r.projectId, Role.Edit).flatMap {
                  case Left(f) => integ.saveRun(r.id, q.taskId, "error", Json.arr(Json.obj("level" -> "error".asJson,
                                    "message" -> s"実行するユーザーに編集権限がありません: ${f.message}".asJson)), Instant.now())
                  case Right(_) => execute(r, Op(s, q.depth + 1), trigger, cond, actions, q.taskId, q.event, q.actorId)
                }
            }
    }

  private def execute(r: RuleRow, op: Op, trigger: Trigger, cond: Option[Condition], actions: List[Action], taskId: Option[UUID],
      event: Json, actorId: Option[UUID]): IO[Unit] =
    val started = Instant.now()
    for
      actor <- actorId.flatTraverse(users.find)
      tctx  <- taskId.flatTraverse(taskContext)
      proj  <- projects.find(r.projectId)
      run = new Run(r, op, Json.obj(
        "task"    -> tctx.getOrElse(Json.Null),
        "actor"   -> actor.fold(Json.Null)(u => Json.obj("id" -> u.id.asJson, "name" -> u.name.asJson, "email" -> u.email.asJson)),
        "event"   -> event.deepMerge(Json.obj("changes" -> changedFields(event).asJson)),
        "webhookData" -> event.hcursor.downField("webhookData").focus.getOrElse(Json.Null),
        "project" -> proj.fold(Json.Null)(p => Json.obj("id" -> p.id.asJson, "key" -> p.key.asJson, "name" -> p.name.asJson)),
        "rule"    -> Json.obj("id" -> r.id.asJson, "name" -> r.name.asJson),
        "vars"    -> Json.obj()
      ), Vector.empty, 0)
      matched = triggerMatches(trigger, event) && cond.forall(c => Automation.eval(c, run.ctx, run.today))
      status <-
        if !matched then IO.pure("no_match")
        else runActions(run, actions).as(if run.log.exists(_.hcursor.get[String]("level").contains("error")) then "error" else "success")
            .handleError { e => run.add("error", s"実行に失敗しました: ${e.getMessage}"); "error" }
      _ <- IO.whenA(status != "no_match")(integ.saveRun(r.id, taskId, status, Json.fromValues(run.log), started))
    yield ()

  private def changedFields(event: Json): List[String] =
    event.hcursor.downField("detail").downField("changes").as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.get[String]("field").toOption)

  private def triggerMatches(t: Trigger, event: Json): Boolean = t match
    case Trigger.TaskUpdated(fields) =>
      fields.isEmpty || changedFields(event).exists(f => fields.exists(x => x == f || f.startsWith(x)))
    case Trigger.StatusChanged(from, to) =>
      val d = event.hcursor.downField("detail")
      def hit(names: List[String], name: String, cat: String) =
        names.isEmpty || names.exists(n => n == name || n == cat || StatusCategory.label(cat) == n)
      hit(from, d.get[String]("from").getOrElse(""), d.get[String]("fromCategory").getOrElse("")) &&
        hit(to, d.get[String]("to").getOrElse(""), d.get[String]("toCategory").getOrElse(""))
    case _ => true

  /** スマート値の task の中身。 */
  def taskContext(id: UUID): IO[Option[Json]] =
    tasks.detail(id).flatMap(_.traverse { d =>
      val t = d.task
      for
        statuses <- projects.statuses(t.projectId)
        labels   <- projects.labels(t.projectId)
        defs     <- projects.fields(t.projectId)
        sprint   <- t.sprintId.flatTraverse(projects.sprint)
        p        <- projects.find(t.projectId)
        a        <- t.assignee.flatTraverse(u => users.find(u.id))
        rp       <- t.reporter.flatTraverse(u => users.find(u.id))
      yield
        val st = statuses.find(_.id == t.statusId)
        def user(u: Option[User]) = u.fold(Json.Null)(x => Json.obj("id" -> x.id.asJson, "name" -> x.name.asJson, "email" -> x.email.asJson))
        Json.obj(
          "id" -> t.id.asJson, "key" -> t.key.asJson, "title" -> t.title.asJson,
          "description" -> d.core.description.fold("")(RichText.toPlain).asJson,
          "statusId" -> t.statusId.asJson,
          "status" -> Json.obj("id" -> t.statusId.asJson, "name" -> st.map(_.name).asJson, "category" -> st.map(_.category).asJson),
          "assignee" -> user(a), "reporter" -> user(rp),
          "labels" -> t.labelIds.flatMap(l => labels.find(_.id == l).map(_.name)).asJson,
          "startDate" -> t.startDate.asJson, "dueDate" -> t.dueDate.asJson,
          "spentMinutes" -> t.spentMinutes.asJson,
          "priority" -> t.priority.asJson, "type" -> t.taskType.asJson,
          "sprint" -> sprint.fold(Json.Null)(s => Json.obj("id" -> s.id.asJson, "name" -> s.name.asJson, "state" -> s.state.asJson)),
          "parent" -> d.ancestors.lastOption.fold(Json.Null)(x => Json.obj("id" -> x.id.asJson, "key" -> x.key.asJson, "title" -> x.title.asJson)),
          "subtasks" -> d.subtasks.map(x => Json.obj("id" -> x.id.asJson, "key" -> x.key.asJson, "title" -> x.title.asJson,
            "status" -> statuses.find(_.id == x.statusId).map(_.name).asJson)).asJson,
          "watchers" -> d.watchers.map(_.name).asJson,
          "links" -> d.links.map(l => Json.obj("kind" -> l.kind.asJson, "key" -> l.target.map(_.key).asJson, "url" -> l.url.asJson)).asJson,
          "fields" -> Json.fromFields(d.fields.toList.flatMap((fid, v) => defs.find(_.id.toString == fid).map(_.name -> v))),
          "commits" -> d.commits.map(c => Json.obj("sha" -> c.sha.asJson, "message" -> c.message.asJson)).asJson,
          "url" -> s"${cfg.publicUrl}/tasks/${t.key}".asJson,
          "project" -> p.fold(Json.Null)(x => Json.obj("key" -> x.key.asJson, "name" -> x.name.asJson)),
          "createdAt" -> t.createdAt.asJson, "updatedAt" -> t.updatedAt.asJson
        )
    })

  private def refresh(run: Run): IO[Unit] =
    run.taskId.flatTraverse(taskContext).map(_.foreach(t => run.ctx = run.ctx.deepMerge(Json.obj("task" -> Json.Null)).deepMerge(Json.obj("task" -> t))))

  private def withTask(run: Run, what: String)(f: TaskCore => IO[Unit]): IO[Unit] =
    run.taskId match
      case None => IO(run.add("warn", s"$what: 対象のタスクがありません"))
      case Some(id) => tasks.core(id).flatMap {
          case None    => IO(run.add("warn", s"$what: タスクが見つかりません"))
          case Some(t) => f(t)
        }

  private def report[A](run: Run, what: String)(r: Res[A]): IO[Unit] =
    r.map {
      case Left(f)  => run.add("error", s"$what: ${f.message}")
      case Right(_) => run.add("info", what)
    }

  private def update(run: Run, what: String, ch: TaskChanges): IO[Unit] =
    withTask(run, what)(t => report(run, what)(svc.update(run.op, t, ch)) *> refresh(run))

  private def userOf(run: Run, spec: String): IO[Option[UUID]] =
    val v = run.render(spec).trim
    v match
      case "" | "none" | "unassigned" => IO.pure(None)
      case "actor" => IO.pure(run.ctx.hcursor.downField("actor").get[UUID]("id").toOption)
      case "reporter" => IO.pure(run.ctx.hcursor.downField("task").downField("reporter").get[UUID]("id").toOption)
      case "assignee" => IO.pure(run.ctx.hcursor.downField("task").downField("assignee").get[UUID]("id").toOption)
      case other => users.resolve(other)

  private def runActions(run: Run, actions: List[Action]): IO[Unit] =
    actions.traverse_(a => if run.steps >= MaxSteps then IO(run.add("error", "アクションの数が上限を超えました")) else IO(run.steps += 1) *> step(run, a))

  private def step(run: Run, a: Action): IO[Unit] = a match
    case Action.SetField(field, value) =>
      val v = value.asString.map(run.render).map(Json.fromString).getOrElse(Automation.renderJson(value, run.ctx, run.today))
      val s = v.asString.map(_.trim)
      def date(what: String) = s.filter(_.nonEmpty) match
        case None    => Right(None)
        case Some(x) => scala.util.Try(LocalDate.parse(x)).toOption.toRight(s"$what を日付として読めません: $x").map(Some(_))
      field match
        case "title" => update(run, s"タイトルを設定", TaskChanges(title = s.filter(_.nonEmpty)))
        case "description" =>
          val doc = s.filter(_.nonEmpty).map(RichText.fromMarkdown)
          update(run, "説明を設定", TaskChanges(description = Some(doc.map(d => d -> RichText.toPlain(d)))))
        case "dueDate" | "startDate" =>
          date(field) match
            case Left(e) => IO(run.add("error", e))
            case Right(d) => update(run, s"${if field == "dueDate" then "期日" else "開始日"}を ${d.fold("なし")(_.toString)} に設定",
              if field == "dueDate" then TaskChanges(dueDate = Some(d)) else TaskChanges(startDate = Some(d)))
        case "priority" => s.flatMap(Priority.parse) match
            case Some(p) => update(run, s"優先度を「${Priority.label(p)}」に設定", TaskChanges(priority = Some(p)))
            case None    => IO(run.add("error", s"優先度は ${Priority.all.mkString(" / ")} です"))
        case "type" | "taskType" => s.flatMap(TaskType.parse) match
            case Some(t) => update(run, s"種類を「${TaskType.label(t)}」に設定", TaskChanges(taskType = Some(t)))
            case None    => IO(run.add("error", s"種類は ${TaskType.all.mkString(" / ")} です"))
        case "assignee" => step(run, Action.Assign(s.getOrElse("")))
        case "reporter" => userOf(run, s.getOrElse("")).flatMap(u => update(run, "報告者を設定", TaskChanges(reporterId = Some(u))))
        case name =>
          withTask(run, s"フィールド「$name」を設定") { t =>
            projects.fields(t.projectId).flatMap(_.find(f => f.name == name || f.id.toString == name) match
              case None => IO(run.add("error", s"フィールド「$name」がありません"))
              case Some(f) =>
                val jv = f.kind match
                  case FieldKind.Checkbox => s.map(x => Json.fromValues(x.split(',').toList.map(_.trim).filter(_.nonEmpty).map(Json.fromString)))
                  case _                  => s.filter(_.nonEmpty).map(Json.fromString)
                update(run, s"フィールド「$name」を設定", TaskChanges(fields = Map(f.id -> jv))))
          }
    case Action.Transition(status) =>
      withTask(run, "ステータスを変更") { t =>
        val name = run.render(status).trim
        projects.statuses(t.projectId).flatMap { ss =>
          ss.find(x => x.name == name || x.id.toString == name).orElse(ss.find(x => x.category == name || StatusCategory.label(x.category) == name)) match
            case None    => IO(run.add("error", s"ステータス「$name」がありません"))
            case Some(x) => update(run, s"ステータスを「${x.name}」に変更", TaskChanges(statusId = Some(x.id)))
        }
      }
    case Action.Assign(user) =>
      userOf(run, user).flatMap { u =>
        if u.isEmpty && !Set("", "none", "unassigned")(run.render(user).trim) then IO(run.add("error", s"ユーザー「${run.render(user)}」が見つかりません"))
        else update(run, u.fold("担当者を外す")(_ => "担当者を設定"), TaskChanges(assigneeId = Some(u)))
      }
    case Action.AddLabel(label) =>
      withTask(run, "ラベルを追加") { t =>
        projects.ensureLabel(t.projectId, run.render(label), None).flatMap { l =>
          tasks.dto(t.id).flatMap(d => update(run, s"ラベル「${l.name}」を追加", TaskChanges(labelIds = Some(d.toList.flatMap(_.labelIds) :+ l.id))))
        }
      }
    case Action.RemoveLabel(label) =>
      withTask(run, "ラベルを外す") { t =>
        projects.labels(t.projectId).flatMap { ls =>
          val n = run.render(label)
          tasks.dto(t.id).flatMap(d => update(run, s"ラベル「$n」を外す",
            TaskChanges(labelIds = Some(d.toList.flatMap(_.labelIds).filterNot(id => ls.exists(l => l.id == id && l.name == n))))))
        }
      }
    case Action.AddWatcher(user) =>
      withTask(run, "ウォッチャーを追加")(t => userOf(run, user).flatMap {
        case None    => IO(run.add("error", s"ユーザー「${run.render(user)}」が見つかりません"))
        case Some(u) => tasks.watch(t.id, u) *> IO(run.add("info", "ウォッチャーを追加"))
      })
    case Action.AddComment(text) =>
      withTask(run, "コメント")(t => report(run, "コメントを追加")(svc.addComment(run.op, t, Json.fromString(run.render(text)))))
    case Action.CreateSubtask(title, assignee, description) =>
      withTask(run, "サブタスクを作成") { t =>
        assignee.traverse(userOf(run, _)).flatMap { a =>
          report(run, s"サブタスク「${run.render(title)}」を作成")(svc.create(run.op, t.projectId, CreateTaskReq(
            title = run.render(title), parentId = Some(t.id), sprintId = t.sprintId, assigneeId = a.flatten,
            description = description.map(d => RichText.fromMarkdown(run.render(d))))))
        } *> refresh(run)
      }
    case Action.CreateTask(title, projectKey, assignee, description, status) =>
      val pidF = projectKey.map(run.render).filter(_.nonEmpty) match
        case Some(k) => projects.findByKey(k).map(_.map(_.id))
        case None    => IO.pure(Some(run.rule.projectId))
      pidF.flatMap {
        case None => IO(run.add("error", s"プロジェクト「${projectKey.getOrElse("")}」が見つかりません"))
        case Some(pid) =>
          for
            a  <- assignee.traverse(userOf(run, _)).map(_.flatten)
            st <- status.traverse(s => projects.statuses(pid).map(_.find(_.name == run.render(s)).map(_.id))).map(_.flatten)
            r  <- svc.create(run.op, pid, CreateTaskReq(title = run.render(title), assigneeId = a, statusId = st,
                    description = description.map(d => RichText.fromMarkdown(run.render(d)))))
            _ <- IO(r match
                   case Left(f)  => run.add("error", s"タスクの作成: ${f.message}")
                   case Right(t) =>
                     run.add("info", s"タスク ${t.key} を作成")
                     run.setVar("createdTask", Json.obj("id" -> t.id.asJson, "key" -> t.key.asJson)))
          yield ()
      }
    case Action.SetSprint(sprint) =>
      withTask(run, "スプリントを設定") { t =>
        val v = run.render(sprint).trim
        projects.sprints(t.projectId, includeClosed = false).flatMap { ss =>
          v match
            case "backlog" | "none" | "バックログ" => update(run, "バックログへ移動", TaskChanges(sprintId = Some(None)))
            case "active" | "アクティブ" =>
              ss.find(_.state == "active").fold(IO(run.add("warn", "進行中のスプリントがありません")))(s =>
                update(run, s"スプリント「${s.name}」へ移動", TaskChanges(sprintId = Some(Some(s.id)))))
            case name =>
              ss.find(s => s.name == name || s.id.toString == name).fold(IO(run.add("error", s"スプリント「$name」がありません")))(s =>
                update(run, s"スプリント「${s.name}」へ移動", TaskChanges(sprintId = Some(Some(s.id)))))
        }
      }
    case Action.SendEmail(to, subject, body) =>
      val addrs = run.render(to).split(',').toList.map(_.trim).filter(_.nonEmpty)
      addrs.traverse_ { a =>
        if a.contains("@") then activity.enqueueMail(None, Some(a), Some(run.rule.projectId), run.render(subject), run.render(body), None, 0)
        else users.resolve(a).flatMap(_.traverse_(u => activity.enqueueMail(Some(u), None, Some(run.rule.projectId), run.render(subject), run.render(body), None, 0)))
      } *> IO(run.add("info", s"メールを送信待ちにしました（${addrs.mkString(", ")}）"))
    case Action.Notify(who, message) =>
      withTask(run, "通知") { t =>
        val c = run.ctx.hcursor.downField("task")
        val targets: IO[Set[UUID]] = run.render(who).split(',').toList.map(_.trim).traverse {
          case "assignee" => IO.pure(c.downField("assignee").get[UUID]("id").toOption.toList)
          case "reporter" => IO.pure(c.downField("reporter").get[UUID]("id").toOption.toList)
          case "watchers" => tasks.watchers(t.id)
          case other      => users.resolve(other).map(_.toList)
        }.map(_.flatten.toSet)
        targets.flatMap(us => notifier.send(NotificationKind.Automation, us, None, t.projectId, Some(t.id), None,
          s"${t.key}: ${run.rule.name}", run.render(message), s"/tasks/${t.key}")) *> IO(run.add("info", "通知を送信"))
      }
    case Action.WebRequest(method, url, headers, body, saveAs) =>
      val u = run.render(url)
      Uri.fromString(u) match
        case Left(_) => IO(run.add("error", s"URL が正しくありません: $u"))
        case Right(uri) =>
          val m    = Method.fromString(method).getOrElse(Method.POST)
          val hs   = headers.toList.map((k, v) => Header.Raw(org.typelevel.ci.CIString(k), run.render(v)))
          val b    = body.map(j => j.asString.map(run.render).getOrElse(Automation.renderJson(j, run.ctx, run.today).noSpaces))
          val req0 = Request[IO](m, uri).putHeaders(hs).putHeaders("User-Agent" -> "skea-automation")
          val req  = b.fold(req0)(x => req0.withEntity(x).withContentType(`Content-Type`(MediaType.application.json)))
          http.run(req).use(r => r.bodyText.compile.string.map(s => (r.status.code, s.take(100000)))).timeout(20.seconds).attempt.map {
            case Left(e) =>
              run.add("error", s"Web リクエスト（$method $u）に失敗しました: ${e.getMessage}")
              run.setVar(saveAs, Json.obj("status" -> 0.asJson, "body" -> Json.Null, "error" -> e.getMessage.asJson))
            case Right((code, text)) =>
              run.add(if code >= 400 then "warn" else "info", s"Web リクエスト $method $u → $code")
              run.setVar(saveAs, Json.obj("status" -> code.asJson, "body" -> parse(text).getOrElse(Json.fromString(text))))
          }
    case Action.SetVar(name, value) => IO(run.setVar(name, Json.fromString(run.render(value))))
    case Action.If(cond, thenDo, elseDo) =>
      val ok = Automation.eval(cond, run.ctx, run.today)
      IO(run.add("info", s"条件: ${if ok then "一致（then）" else "不一致（else）"}")) *> runActions(run, if ok then thenDo else elseDo)
    case Action.ForEach(target, acts) =>
      run.taskId match
        case None => IO(run.add("warn", "for_each: 対象のタスクがありません"))
        case Some(id) =>
          val idsF: IO[List[UUID]] = target match
            case "subtasks" => tasks.subtaskIds(id)
            case "parent"   => tasks.core(id).map(_.flatMap(_.parentId).toList)
            case _          => tasks.linkedIds(id)
          idsF.flatMap { ids =>
            val saved = run.ctx
            ids.traverse_ { cid =>
              taskContext(cid).flatMap {
                case None => IO.unit
                case Some(tc) =>
                  IO { run.ctx = saved.deepMerge(Json.obj("task" -> Json.Null)).deepMerge(Json.obj("task" -> tc)) } *> runActions(run, acts)
              }
            } *> IO { run.ctx = saved } *> refresh(run)
          }
    case Action.Log(message) => IO(run.add("info", run.render(message)))
