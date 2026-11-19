package skea.core

import io.circe.{ACursor, Json}

import java.time.{LocalDate, LocalTime}

/**
 * 自動化（オートメーション）のルールの定義と、条件の評価・スマート値の展開（副作用なし）。
 * ルールは JSON で保存し、実行時にここで読み込む。実行（タスクの更新など）は api モジュールの AutomationEngine が行う。
 */
object Automation {

  // ------------------------------------------------------------------ トリガー

  enum Trigger {
    case TaskCreated

    /** fields が空なら、どのフィールドの変更でも起動する。 */
    case TaskUpdated(
      fields: List[String])

    /** from / to はステータス名またはカテゴリ（空なら指定なし）。 */
    case StatusChanged(
      from: List[String],
      to: List[String])
    case CommentAdded
    case SprintStarted
    case SprintCompleted
    case CommitLinked
    case WorklogAdded

    /** 毎日 time に（daily）、または intervalMinutes ごとに。query を指定すると一致したタスクごとに実行する。 */
    case Scheduled(
      daily: Option[LocalTime],
      intervalMinutes: Option[Int],
      query: Option[String])
    case Manual
    case IncomingWebhook(
      query: Option[String])
  }

  object Trigger {

    /** イベント名（task_event.kind）と対応するトリガーの種類。 */
    def eventType(kind: String): Option[String] = kind match {
      case "task.created" => Some("task_created")
      case "task.updated" => Some("task_updated")
      case "task.status_changed" => Some("status_changed")
      case "comment.added" => Some("comment_added")
      case "sprint.started" => Some("sprint_started")
      case "sprint.completed" => Some("sprint_completed")
      case "commit.linked" => Some("commit_linked")
      case "worklog.added" => Some("worklog_added")
      case _ => None
    }
  }

  def typeOf(t: Trigger): String = t match {
    case Trigger.TaskCreated => "task_created"
    case _: Trigger.TaskUpdated => "task_updated"
    case _: Trigger.StatusChanged => "status_changed"
    case Trigger.CommentAdded => "comment_added"
    case Trigger.SprintStarted => "sprint_started"
    case Trigger.SprintCompleted => "sprint_completed"
    case Trigger.CommitLinked => "commit_linked"
    case Trigger.WorklogAdded => "worklog_added"
    case _: Trigger.Scheduled => "scheduled"
    case Trigger.Manual => "manual"
    case _: Trigger.IncomingWebhook => "incoming_webhook"
  }

  private def strs(c: ACursor, k: String): List[String] =
    c.downField(
      k
    ).as[List[String]].orElse(c.downField(k).as[String].map(List(_))).getOrElse(Nil).map(_.trim).filter(_.nonEmpty)

  def parseTrigger(j: Json): Either[String, (Trigger, Option[Condition])] = {
    val c = j.hcursor
    val cond = c.downField("condition").focus.filterNot(_.isNull) match {
      case Some(cj) => parseCondition(cj).map(Some(_))
      case None => Right(None)
    }
    cond.flatMap { cd =>
      (c.get[String]("type").getOrElse("") match {
        case "task_created" => Right(Trigger.TaskCreated)
        case "task_updated" => Right(Trigger.TaskUpdated(strs(c, "fields")))
        case "status_changed" => Right(Trigger.StatusChanged(strs(c, "from"), strs(c, "to")))
        case "comment_added" => Right(Trigger.CommentAdded)
        case "sprint_started" => Right(Trigger.SprintStarted)
        case "sprint_completed" => Right(Trigger.SprintCompleted)
        case "commit_linked" => Right(Trigger.CommitLinked)
        case "worklog_added" => Right(Trigger.WorklogAdded)
        case "scheduled" =>
          val daily = c.get[String]("time").toOption.flatMap(t => scala.util.Try(LocalTime.parse(t)).toOption)
          val every = c.get[Int]("intervalMinutes").toOption.filter(_ >= 5)
          if daily.isEmpty && every.isEmpty then Left("定期トリガーには time（HH:MM）か intervalMinutes（5以上）が必要です")
          else Right(Trigger.Scheduled(daily, every, c.get[String]("query").toOption.filter(_.trim.nonEmpty)))
        case "manual" => Right(Trigger.Manual)
        case "incoming_webhook" =>
          Right(Trigger.IncomingWebhook(c.get[String]("query").toOption.filter(_.trim.nonEmpty)))
        case other => Left(s"不明なトリガーです: $other")
      }).map(_ -> cd)
    }
  }

  // ------------------------------------------------------------------ 条件

  enum Condition {
    case All(
      cs: List[Condition])
    case AnyOf(
      cs: List[Condition])
    case Not(
      c: Condition)

    /** path はスマート値のパス（task.status.category など）。value はテンプレート（{{...}} を展開してから比べる）。 */
    case Compare(
      path: String,
      op: String,
      value: Json)
  }

  val ops: List[String] = List(
    "eq",
    "neq",
    "contains",
    "not_contains",
    "in",
    "not_in",
    "empty",
    "not_empty",
    "gt",
    "gte",
    "lt",
    "lte",
    "matches",
    "changed"
  )

  def parseCondition(j: Json): Either[String, Condition] = {
    val c = j.hcursor
    def list(k: String) = c.downField(k).as[List[Json]].left.map(_ => s"$k は配列で指定してください")
      .flatMap(_.foldRight[Either[String, List[Condition]]](Right(Nil))((x, acc) =>
        acc.flatMap(l => parseCondition(x).map(_ :: l))
      ))
    if c.downField("all").succeeded then list("all").map(Condition.All(_))
    else if c.downField("any").succeeded then list("any").map(Condition.AnyOf(_))
    else if c.downField("not").succeeded then
      c.downField("not").focus.toRight("not").flatMap(parseCondition).map(Condition.Not(_))
    else
      for {
        p <- c.get[String]("field").left.map(_ => "条件に field がありません")
        op <- c.get[String]("op").orElse(Right("eq")).flatMap(o =>
          if ops.contains(o) then Right(o) else Left(io.circe.DecodingFailure(o, Nil))
        )
          .left.map(_ => s"不明な比較です: ${c.get[String]("op").getOrElse("")}")
      } yield Condition.Compare(p, op, c.downField("value").focus.getOrElse(Json.Null))
  }

  /** ctx はスマート値の元になる JSON（task / actor / event / vars / webResponse …）。 */
  def eval(cond: Condition, ctx: Json, today: LocalDate): Boolean = cond match {
    case Condition.All(cs) => cs.forall(eval(_, ctx, today))
    case Condition.AnyOf(cs) => cs.exists(eval(_, ctx, today))
    case Condition.Not(c) => !eval(c, ctx, today)
    case Condition.Compare(path, op, raw) =>
      val actual = lookup(ctx, path)
      val expected = raw.asString.map(s => Json.fromString(render(s, ctx, today))).getOrElse(raw)
      compare(actual, op, expected, ctx, path)
  }

  private def str(j: Json): String =
    j.asString.orElse(
      j.asNumber.map(n => n.toLong.fold(n.toDouble.toString)(_.toString))
    ).orElse(j.asBoolean.map(_.toString))
      .getOrElse(if j.isNull then "" else j.noSpaces)

  private def num(j: Json): Option[BigDecimal] =
    j.asNumber.flatMap(_.toBigDecimal).orElse(j.asString.flatMap(s => scala.util.Try(BigDecimal(s.trim)).toOption))

  private def elems(j: Json): List[Json] = j.asArray.map(_.toList).getOrElse(if j.isNull then Nil else List(j))

  private def isEmpty(j: Json): Boolean =
    j.isNull || j.asString.exists(_.trim.isEmpty) || j.asArray.exists(_.isEmpty) || j.asObject.exists(_.isEmpty)

  private def cmp(a: Json, b: Json): Option[Int] =
    (num(a), num(b)) match {
      case (Some(x), Some(y)) => Some(x.compare(y))
      case _ =>
        val (x, y) = (str(a), str(b))
        if x.isEmpty || y.isEmpty then None else Some(x.compareTo(y)) // 日付（yyyy-MM-dd）・日時は文字列のまま比べられる
    }

  private def eqLoose(a: Json, b: Json): Boolean =
    (num(a), num(b)) match {
      case (Some(x), Some(y)) => x == y
      case _ => str(a).trim.equalsIgnoreCase(str(b).trim)
    }

  private def compare(actual: Json, op: String, expected: Json, ctx: Json, path: String): Boolean = {
    val expList = expected.asArray.map(_.toList).getOrElse(expected.asString.map(_.split(',').toList.map(s =>
      Json.fromString(s.trim)
    )).getOrElse(List(expected)))
    op match {
      case "eq" => if actual.isArray then elems(actual).exists(eqLoose(_, expected)) else eqLoose(actual, expected)
      case "neq" => !(if actual.isArray then elems(actual).exists(eqLoose(_, expected)) else eqLoose(actual, expected))
      case "contains" =>
        if actual.isArray then elems(actual).exists(eqLoose(_, expected))
        else str(actual).toLowerCase.contains(str(expected).toLowerCase)
      case "not_contains" => !compare(actual, "contains", expected, ctx, path)
      case "in" => elems(actual).exists(a => expList.exists(eqLoose(a, _)))
      case "not_in" => !elems(actual).exists(a => expList.exists(eqLoose(a, _)))
      case "empty" => isEmpty(actual)
      case "not_empty" => !isEmpty(actual)
      case "gt" => cmp(actual, expected).exists(_ > 0)
      case "gte" => cmp(actual, expected).exists(_ >= 0)
      case "lt" => cmp(actual, expected).exists(_ < 0)
      case "lte" => cmp(actual, expected).exists(_ <= 0)
      case "matches" => scala.util.Try(str(expected).r.findFirstIn(str(actual)).isDefined).getOrElse(false)
      case "changed" =>
        // event.changes に、path の task. を外したフィールド名が含まれるか
        val field = path.stripPrefix("task.")
        elems(lookup(ctx, "event.changes")).exists(c => str(c) == field || field.startsWith(str(c) + "."))
      case _ => false
    }
  }

  // ------------------------------------------------------------------ スマート値

  /** ドット区切りのパスで JSON をたどる（配列は .0 などの番号か、.size）。 */
  def lookup(ctx: Json, path: String): Json =
    path.trim.split('.').toList.filter(_.nonEmpty).foldLeft(ctx) {
      (j, k) =>
        if j.isArray && k == "size" then Json.fromInt(j.asArray.get.size)
        else if j.isArray && k.forall(_.isDigit) then j.asArray.get.lift(k.toInt).getOrElse(Json.Null)
        else if j.isArray then
          Json.fromValues(j.asArray.get.map(e => e.hcursor.downField(k).focus.getOrElse(Json.Null)))
        else if j.isString && k == "size" then Json.fromInt(j.asString.get.length)
        else j.hcursor.downField(k).focus.getOrElse(Json.Null)
    }

  private val Tpl = """\{\{\s*([^}|]+?)\s*(?:\|\s*([^}]*?))?\s*\}\}""".r

  /** `{{task.key}}`・`{{task.assignee.name | default:未割り当て}}`・`{{today+7}}`・`{{now}}` を展開する。 */
  def render(s: String, ctx: Json, today: LocalDate): String =
    Tpl.replaceAllIn(
      s,
      m => {
        val path = m.group(1).trim
        val filt = Option(m.group(2)).map(_.trim).filter(_.nonEmpty)
        val v = special(path, today).getOrElse(lookup(ctx, path))
        val s0 = v.asArray match {
          case Some(a) => a.map(str).filter(_.nonEmpty).mkString(", ")
          case None => str(v)
        }
        val out = filt.foldLeft(s0)(applyFilter)
        scala.util.matching.Regex.quoteReplacement(out)
      }
    )

  private def special(path: String, today: LocalDate): Option[Json] = {
    val re = """^today\s*([+-])\s*(\d+)$""".r
    path match {
      case "today" => Some(Json.fromString(today.toString))
      case "now" => Some(Json.fromString(java.time.Instant.now().toString))
      case re(sign, n) =>
        Some(Json.fromString((if sign == "+" then today.plusDays(n.toLong) else today.minusDays(n.toLong)).toString))
      case _ => None
    }
  }

  private def applyFilter(v: String, f: String): String = f match {
    case s"default:$d" => if v.trim.isEmpty then d.trim else v
    case "upper" => v.toUpperCase
    case "lower" => v.toLowerCase
    case "trim" => v.trim
    case "urlencode" => java.net.URLEncoder.encode(v, "UTF-8")
    case "json" => Json.fromString(v).noSpaces
    case s"left:$n" => n.trim.toIntOption.fold(v)(v.take)
    case _ => v
  }

  /** JSON の値の中の文字列をすべて展開する（Web リクエストの本文など）。 */
  def renderJson(j: Json, ctx: Json, today: LocalDate): Json =
    j.fold(
      Json.Null,
      Json.fromBoolean,
      Json.fromJsonNumber,
      s => Json.fromString(render(s, ctx, today)),
      a => Json.fromValues(a.map(renderJson(_, ctx, today))),
      o => Json.fromJsonObject(o.mapValues(renderJson(_, ctx, today)))
    )

  // ------------------------------------------------------------------ アクション

  enum Action {
    case SetField(
      field: String,
      value: Json)
    case Transition(
      status: String)
    case Assign(
      user: String)
    case AddLabel(
      label: String)
    case RemoveLabel(
      label: String)
    case AddWatcher(
      user: String)
    case AddComment(
      text: String)
    case CreateSubtask(
      title: String,
      assignee: Option[String],
      description: Option[String])
    case CreateTask(
      title: String,
      projectKey: Option[String],
      assignee: Option[String],
      description: Option[String],
      status: Option[String])
    case SetSprint(
      sprint: String)
    case SendEmail(
      to: String,
      subject: String,
      body: String)
    case Notify(
      users: String,
      message: String)
    case WebRequest(
      method: String,
      url: String,
      headers: Map[String, String],
      body: Option[Json],
      saveAs: String)
    case SetVar(
      name: String,
      value: String)
    case If(
      cond: Condition,
      thenDo: List[Action],
      elseDo: List[Action])
    case ForEach(
      target: String,
      actions: List[Action])
    case Log(
      message: String)
  }

  val actionTypes: List[String] = List(
    "set_field",
    "transition",
    "assign",
    "add_label",
    "remove_label",
    "add_watcher",
    "comment",
    "create_subtask",
    "create_task",
    "set_sprint",
    "send_email",
    "notify",
    "web_request",
    "set_var",
    "if",
    "for_each",
    "log"
  )

  def parseActions(j: Json): Either[String, List[Action]] =
    j.asArray.toRight("アクションは配列で指定してください").flatMap(
      _.toList.foldRight[Either[String, List[Action]]](Right(Nil))((x, acc) =>
        acc.flatMap(l => parseAction(x).map(_ :: l))
      )
    )

  private def req(c: ACursor, k: String, t: String): Either[String, String] =
    c.get[String](k).toOption.filter(_.trim.nonEmpty).toRight(s"$t に $k がありません")

  def parseAction(j: Json): Either[String, Action] = {
    val c = j.hcursor
    val o = (k: String) => c.get[String](k).toOption.filter(_.trim.nonEmpty)
    c.get[String]("type").getOrElse("") match {
      case "set_field" =>
        req(c, "field", "set_field").map(f => Action.SetField(f, c.downField("value").focus.getOrElse(Json.Null)))
      case "transition" => req(c, "status", "transition").map(Action.Transition(_))
      case "assign" => Right(Action.Assign(o("user").getOrElse("")))
      case "add_label" => req(c, "label", "add_label").map(Action.AddLabel(_))
      case "remove_label" => req(c, "label", "remove_label").map(Action.RemoveLabel(_))
      case "add_watcher" => req(c, "user", "add_watcher").map(Action.AddWatcher(_))
      case "comment" => req(c, "text", "comment").map(Action.AddComment(_))
      case "create_subtask" =>
        req(c, "title", "create_subtask").map(t => Action.CreateSubtask(t, o("assignee"), o("description")))
      case "create_task" => req(c, "title", "create_task").map(t =>
          Action.CreateTask(t, o("projectKey"), o("assignee"), o("description"), o("status"))
        )
      case "set_sprint" => req(c, "sprint", "set_sprint").map(Action.SetSprint(_))
      case "send_email" =>
        for to <- req(c, "to", "send_email"); s <- req(c, "subject", "send_email")
        yield Action.SendEmail(to, s, c.get[String]("body").getOrElse(""))
      case "notify" => req(c, "message", "notify").map(m => Action.Notify(o("users").getOrElse("assignee"), m))
      case "web_request" =>
        req(c, "url", "web_request").map { u =>
          val method =
            o("method").map(_.toUpperCase).filter(Set("GET", "POST", "PUT", "PATCH", "DELETE")).getOrElse("POST")
          val headers = c.downField("headers").as[Map[String, String]].getOrElse(Map.empty)
          Action.WebRequest(
            method,
            u,
            headers,
            c.downField("body").focus.filterNot(_.isNull),
            o("saveAs").getOrElse("webResponse")
          )
        }
      case "set_var" => req(c, "name", "set_var").map(n => Action.SetVar(n, c.get[String]("value").getOrElse("")))
      case "if" =>
        for {
          cj <- c.downField("condition").focus.toRight("if に condition がありません")
          cd <- parseCondition(cj)
          th <- parseActions(c.downField("then").focus.getOrElse(Json.arr()))
          el <- parseActions(c.downField("else").focus.getOrElse(Json.arr()))
        } yield Action.If(cd, th, el)
      case "for_each" =>
        val target = o("target").getOrElse("subtasks")
        if !Set("subtasks", "parent", "linked")(target) then
          Left(s"for_each の target は subtasks / parent / linked です: $target")
        else parseActions(c.downField("actions").focus.getOrElse(Json.arr())).map(Action.ForEach(target, _))
      case "log" => Right(Action.Log(c.get[String]("message").getOrElse("")))
      case other => Left(s"不明なアクションです: $other")
    }
  }

  /** ルール全体の検証（保存時）。 */
  def validate(trigger: Json, actions: Json): Either[String, Unit] =
    for {
      _ <- parseTrigger(trigger)
      a <- parseActions(actions)
      _ <- if a.isEmpty then Left("アクションを1つ以上指定してください") else Right(())
    } yield ()

  /** 定期トリガーを今動かすべきか。last は前回の実行（なければ作成時刻）。 */
  def scheduledDue(t: Trigger.Scheduled,
                   last: java.time.Instant,
                   now: java.time.Instant,
                   zone: java.time.ZoneId): Boolean =
    t.intervalMinutes match {
      case Some(m) => !last.plusSeconds(m.toLong * 60).isAfter(now)
      case None =>
        t.daily.exists { time =>
          val todayAt = now.atZone(zone).toLocalDate.atTime(time).atZone(zone).toInstant
          !todayAt.isAfter(now) && last.isBefore(todayAt)
        }
    }
}
