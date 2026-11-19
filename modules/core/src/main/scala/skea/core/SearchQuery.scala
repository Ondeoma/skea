package skea.core

import java.time.LocalDate

/**
 * 検索欄の絞り込み構文。
 * 例: `project:SKEA status:進行中 assignee:me label:バグ sprint:active due<2026-12-01 updated>-7d is:open "ログイン 画面" -label:保留 order:-updated`
 *
 *  - `キー:値`、`キー<値`、`キー>値`、`キー<=値`、`キー>=値`。先頭の `-` は否定。
 *  - 値に空白を含めるときは "..." で囲む。キーのない語は全文検索（AND）。
 *  - 日付は `2026-12-01`、`today`、`-7d`（7日前）、`+3d`、`-2w`、`-1m`。
 */
object SearchQuery {

  enum Op {
    case Eq, Lt, Gt, Le, Ge
  }

  final case class Filter(key: String, op: Op, value: String, negate: Boolean)
  final case class Query(filters: List[Filter],
                         text: List[String],
                         negText: List[String],
                         order: List[(String, Boolean)]) {
    def isEmpty: Boolean = filters.isEmpty && text.isEmpty && negText.isEmpty
  }

  /** 使えるキー（別名を含む）。 */
  val keys: Map[String, String] = Map(
    "project" -> "project",
    "p" -> "project",
    "プロジェクト" -> "project",
    "status" -> "status",
    "ステータス" -> "status",
    "category" -> "category",
    "cat" -> "category",
    "カテゴリ" -> "category",
    "assignee" -> "assignee",
    "担当" -> "assignee",
    "担当者" -> "assignee",
    "reporter" -> "reporter",
    "報告者" -> "reporter",
    "watcher" -> "watcher",
    "ウォッチャー" -> "watcher",
    "label" -> "label",
    "ラベル" -> "label",
    "priority" -> "priority",
    "優先度" -> "priority",
    "type" -> "type",
    "種類" -> "type",
    "sprint" -> "sprint",
    "スプリント" -> "sprint",
    "due" -> "due",
    "期日" -> "due",
    "start" -> "start",
    "開始" -> "start",
    "created" -> "created",
    "作成" -> "created",
    "updated" -> "updated",
    "更新" -> "updated",
    "resolved" -> "resolved",
    "完了日" -> "resolved",
    "is" -> "is",
    "has" -> "has",
    "parent" -> "parent",
    "親" -> "parent",
    "key" -> "key",
    "id" -> "key",
    "field" -> "field",
    "f" -> "field",
    "order" -> "order",
    "sort" -> "order",
    "並び" -> "order",
    "text" -> "text",
    "title" -> "title",
    "タイトル" -> "title",
    "commented" -> "commented",
    "comment" -> "comment",
    "コメント" -> "comment"
  )

  val orderKeys: Set[String] = Set(
    "due",
    "start",
    "created",
    "updated",
    "resolved",
    "key",
    "rank",
    "title",
    "status",
    "assignee",
    "priority",
    "type"
  )

  /** 字句に分ける（"..." は1語）。 */
  private[core] def tokens(s: String): List[String] = {
    val out = List.newBuilder[String]
    val cur = new StringBuilder
    var inQ = false
    s.foreach { c =>
      if c == '"' then inQ = !inQ
      else if (c == ' ' || c == '　' || c == '\t') && !inQ then {
        if cur.nonEmpty then { out += cur.toString; cur.clear() }
      } else cur.append(c)
    }
    if cur.nonEmpty then out += cur.toString
    out.result()
  }

  private val FilterRe = """^(-?)([^\s:<>=]+?)(:|<=|>=|<|>)(.*)$""".r

  def parse(s: String): Either[String, Query] = {
    val ts = tokens(s)
    val filters = List.newBuilder[Filter]
    val text = List.newBuilder[String]
    val neg = List.newBuilder[String]
    val order = List.newBuilder[(String, Boolean)]
    var error: Option[String] = None
    ts.foreach { t =>
      t match {
        case FilterRe(n, k, op, v) if keys.contains(k.toLowerCase) || keys.contains(k) =>
          val key = keys.getOrElse(k.toLowerCase, keys(k))
          val o = op match {
            case ":" => Op.Eq
            case "<" => Op.Lt
            case ">" => Op.Gt
            case "<=" => Op.Le
            case ">=" => Op.Ge
          }
          if v.isEmpty then error = error.orElse(Some(s"「$t」の値がありません"))
          else if key == "order" then
            v.split(',').foreach { x =>
              val desc = x.startsWith("-")
              val f = x.stripPrefix("-").stripPrefix("+").toLowerCase
              if orderKeys(f) then order += f -> !desc
              else error = error.orElse(Some(s"並び順に使えない項目です: $f"))
            }
          else if key == "text" || key == "title" then
            if n == "-" then neg += v else text += v
          else filters += Filter(key, o, v, n == "-")
        case _ =>
          if t.startsWith("-") && t.length > 1 then neg += t.drop(1)
          else text += t
      }
    }
    error.toLeft(Query(filters.result(), text.result(), neg.result(), order.result()))
  }

  /** 日付の値（today、-7d、+2w、-1m、2026-12-01）。 */
  def date(v: String, today: LocalDate): Option[LocalDate] = {
    val t = v.trim.toLowerCase
    if t == "today" || t == "今日" then Some(today)
    else if t == "tomorrow" || t == "明日" then Some(today.plusDays(1))
    else if t == "yesterday" || t == "昨日" then Some(today.minusDays(1))
    else {
      val re = """^([+-]?)(\d+)([dwmy])$""".r
      t match {
        case re(sign, n, unit) =>
          val k = n.toLong * (if sign == "-" then -1 else 1)
          Some(unit match {
            case "d" => today.plusDays(k)
            case "w" => today.plusWeeks(k)
            case "m" => today.plusMonths(k)
            case _ => today.plusYears(k)
          })
        case _ => scala.util.Try(LocalDate.parse(t)).toOption
      }
    }
  }

  /** 1つの語を PGroonga のクエリ構文のフレーズにする（構文の注入を防ぐ）。 */
  def phrase(w: String): String = "\"" + w.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
}
