package skea.web

import skea.shared.*

import java.util.UUID

/** ヘルプデスクの"ユーザ"のグループ（親子の階層）の表示の補助. */
object HelpGroups {

  /** 親から順に並べ、深さを付ける（親が見つからないものは最上位に置く）. */
  def tree(gs: List[RequesterGroupDto]): List[(RequesterGroupDto, Int)] = {
    val ids = gs.map(_.id).toSet
    def children(p: Option[UUID]): List[RequesterGroupDto] =
      gs.filter(g => g.parentId.filter(ids.contains) == p).sortBy(_.name)
    def walk(p: Option[UUID], depth: Int, seen: Set[UUID]): List[(RequesterGroupDto, Int)] =
      children(p).filterNot(g => seen(g.id)).flatMap(g => (g, depth) :: walk(Some(g.id), depth + 1, seen + g.id))
    walk(None, 0, Set.empty)
  }

  /** 「A社 › 経理部」. */
  def path(gs: List[RequesterGroupDto], g: RequesterGroupDto): String = RequesterGroups.path(gs, g)

  /** 共有先・親の選択肢（グループは階層の名前で）. */
  def options(gs: List[RequesterGroupDto]): List[Opt] =
    tree(gs).map((g, _) => Opt(g.id.toString, path(gs, g), icon = "👥"))

  /** ステータスの色付きの表示（窓口の一覧・詳細）. */
  def statusStyle(t: HelpTaskDto): String = s"background:${t.statusColor}"
}

/** 窓口の詳細検索の条件（画面の入力欄）. build で検索欄の構文にする（構文は画面に出し、そのまま直せる）. */
final case class HelpCond(
    words: String = "",
    scope: String = "",               // "" すべて / mine 自分の問い合わせ / shared 共有されたもの
    categories: List[String] = Nil,   // todo・in_progress・waiting・done
    statuses: List[String] = Nil,     // ステータスの名前
    assignee: String = "",            // 名前・none（未割り当て）
    reporter: String = "",
    createdFrom: String = "", createdTo: String = "",
    updatedFrom: String = "", updatedTo: String = "",
    attachment: Boolean = false,
    replies: Boolean = false,
    watching: Boolean = false,
    order: String = "-updated"
)

object HelpQuery {

  /** 空白を含む値は "..." で囲む（検索の構文の1語にする）. */
  def quote(v: String): String = if (v.exists(c => c == ' ' || c == '　' || c == ',')) "\"" + v.replace("\"", "") + "\"" else v

  def build(c: HelpCond): String = List(
    Some(c.words.trim).filter(_.nonEmpty),
    Option.when(c.scope.nonEmpty)("is:" + c.scope),
    Option.when(c.categories.nonEmpty)("category:" + c.categories.mkString(",")),
    Option.when(c.statuses.nonEmpty)("status:" + c.statuses.map(quote).mkString(",")),
    Some(c.assignee.trim).filter(_.nonEmpty).map(a => "assignee:" + quote(a)),
    Some(c.reporter.trim).filter(_.nonEmpty).map(a => "reporter:" + quote(a)),
    Some(c.createdFrom).filter(_.nonEmpty).map("created>=" + _),
    Some(c.createdTo).filter(_.nonEmpty).map("created<=" + _),
    Some(c.updatedFrom).filter(_.nonEmpty).map("updated>=" + _),
    Some(c.updatedTo).filter(_.nonEmpty).map("updated<=" + _),
    Option.when(c.attachment)("has:attachment"),
    Option.when(c.replies)("has:comments"),
    Option.when(c.watching)("is:watching"),
    Some(c.order).filter(o => o.nonEmpty && o != "-updated").map("order:" + _)
  ).flatten.mkString(" ")

  /** よく使う絞り込み（ボタン）. */
  val presets: List[(String, String)] = List(
    "is:open" -> "未完了",
    "is:mine is:open" -> "自分の問い合わせ",
    "is:shared" -> "共有されたもの",
    "is:watching is:open" -> "通知を受け取っている",
    "updated>=-7d" -> "1週間以内に更新",
    "is:done" -> "完了",
    "" -> "すべて"
  )

  val orders: List[(String, String)] = List(
    "-updated" -> "更新が新しい順", "updated" -> "更新が古い順", "-created" -> "作成が新しい順", "created" -> "作成が古い順",
    "-resolved" -> "完了が新しい順", "title" -> "件名順", "status" -> "ステータス順", "-key" -> "番号が新しい順"
  )

  val categories: List[(String, String)] = List(
    StatusCategory.Todo -> "作業前", StatusCategory.InProgress -> "進行中", StatusCategory.Waiting -> "待機中", StatusCategory.Done -> "完了")
}

/** 窓口の問い合わせの項目の値の表示（文字列・選んだもの）. */
object HelpFields {
  def show(v: io.circe.Json): String =
    v.asString.orElse(v.asArray.map(_.flatMap(_.asString).mkString("、"))).getOrElse(v.noSpaces)
}
