package skea.db

import sbirds.db.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skea.core.*
import skea.core.SearchQuery.Op
import skea.shared.*

import java.time.LocalDate
import java.util.UUID

/**
 * 検索の SQL の部品（エージェントの検索と、ヘルプデスクの窓口の検索で共通）。テーブルの別名は t（task）・s（status）。
 * publicOnly なら、コメントは公開のもの（内部コメントを除く）だけを見る（"ユーザ"に内部コメントの中身や有無を漏らさない）。
 */
object SearchSql {

  /** タイプミスを許容する度合い（バイグラムの2文字の語で1文字違いまで）。 */
  val FuzzyRatio = 0.5

  def values(v: String): List[String] = v.split(',').toList.map(_.trim).filter(_.nonEmpty)

  def userMatch(col: Fragment, v: String, me: UUID): Fragment =
    Sql.or(values(v).map {
      case "me" | "自分" | "currentuser()" => col ++ fr"= $me"
      case "none" | "なし" | "empty" => col ++ fr"is null"
      case x =>
        uuidOpt(x) match {
          case Some(id) => col ++ fr"= $id"
          case None => fr"exists(select 1 from app_user u where u.id =" ++ col ++
              fr"and (u.name ilike ${"%" + x + "%"} or lower(u.email) = lower($x) or u.sub = $x))"
        }
    })

  def dateCol(col: Fragment, op: Op, v: String, today: LocalDate): Either[String, Fragment] =
    if v == "none" || v == "なし" then Right(col ++ fr"is null")
    else
      SearchQuery.date(v, today).toRight(s"日付として読めません: $v").map { d =>
        op match {
          case Op.Eq => col ++ fr"::date = $d"
          case Op.Lt => col ++ fr"::date < $d"
          case Op.Le => col ++ fr"::date <= $d"
          case Op.Gt => col ++ fr"::date > $d"
          case Op.Ge => col ++ fr"::date >= $d"
        }
      }

  val categoryAlias: Map[String, String] = Map(
    "todo" -> StatusCategory.Todo,
    "作業前" -> StatusCategory.Todo,
    "未着手" -> StatusCategory.Todo,
    "in_progress" -> StatusCategory.InProgress,
    "inprogress" -> StatusCategory.InProgress,
    "進行中" -> StatusCategory.InProgress,
    "waiting" -> StatusCategory.Waiting,
    "待機中" -> StatusCategory.Waiting,
    "保留" -> StatusCategory.Waiting,
    "done" -> StatusCategory.Done,
    "完了" -> StatusCategory.Done
  )

  def category(v: String): Either[String, Fragment] =
    values(v).traverse(x =>
      if x == "open" then Right("open")
      else categoryAlias.get(x.toLowerCase).toRight(s"カテゴリは todo / in_progress / waiting / done です: $x")
    )
      .map(cs => Sql.or(cs.map(c => if c == "open" then fr"s.category <> 'done'" else fr"s.category = $c")))

  def status(v: String): Fragment =
    Sql.or(values(v).map(x => uuidOpt(x).fold(fr"lower(s.name) = lower($x)")(id => fr"s.id = $id")))

  def key(v: String): Fragment = fr"t.key = any(${values(v).map(_.toUpperCase).toArray})"

  private def commentScope(publicOnly: Boolean): Fragment =
    if publicOnly then fr"c.deleted_at is null and not c.internal" else fr"c.deleted_at is null"

  def commented(v: String, me: UUID, publicOnly: Boolean): Fragment =
    fr"exists(select 1 from comment c where c.task_id = t.id and" ++ commentScope(publicOnly) ++ fr"and" ++
      userMatch(fr"c.author_id", v, me) ++ fr")"

  def comment(v: String, publicOnly: Boolean): Fragment =
    fr"exists(select 1 from comment c where c.task_id = t.id and" ++ commentScope(publicOnly) ++
      fr"and c.body_text &@~ ${SearchQuery.phrase(v)})"

  /** 全文検索（タスクの key・タイトル・説明と、コメントをそれぞれのインデックスで探す. OR で1つの条件にするとインデックスが使われない）。 */
  def textCond(text: String, fuzzy: Boolean, publicOnly: Boolean): Fragment = {
    val ratio = if fuzzy then FuzzyRatio else 0.0
    fr"""t.id in (select x.id from task x where array[x.key, x.title, x.description_text] &@~
                   pgroonga_condition($text, index_name => 'task_search_idx', fuzzy_max_distance_ratio => $ratio::real)
                 union select c.task_id from comment c where""" ++ commentScope(publicOnly) ++
      fr"""and c.body_text &@~
                   pgroonga_condition($text, index_name => 'comment_search_idx', fuzzy_max_distance_ratio => $ratio::real))"""
  }

  def orderCol(k: String): Fragment = k match {
    case "due" => fr"t.due_date"
    case "start" => fr"t.start_date"
    case "created" => fr"t.created_at"
    case "updated" => fr"t.updated_at"
    case "resolved" => fr"t.resolved_at"
    case "key" => fr"t.project_id, t.number"
    case "rank" => fr"""t.rank collate "C""""
    case "title" => fr"t.title"
    case "status" => fr"s.position"
    case "priority" => fr"array_position(array['lowest','low','medium','high','highest'], t.priority)"
    case "type" => fr"t.type"
    case _ => fr"(select name from app_user where id = t.assignee_id)"
  }
}
