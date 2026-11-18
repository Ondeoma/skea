package skea.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.postgres.circe.jsonb.implicits.*
import io.circe.Json
import io.circe.syntax.*
import skea.core.*
import skea.core.SearchQuery.{Filter, Op}
import skea.shared.*

import java.time.LocalDate
import java.util.UUID

/** タスクの検索（絞り込み構文 + PGroonga の全文検索）。 */
final class SearchStore(xa: Transactor[IO], tasks: TaskStore) extends TxStore(xa):

  final case class Ctx(userId: UUID, projectIds: List[UUID], today: LocalDate)

  private def filterSql(f: Filter, ctx: Ctx): Either[String, Fragment] =
    val v = f.value
    val res: Either[String, Fragment] = f.key match
      case "project" =>
        Right(Sql.or(SearchSql.values(v).map(x => fr"p.key = ${x.toUpperCase} or p.name = $x")))
      case "status" => Right(SearchSql.status(v))
      case "category" => SearchSql.category(v)
      case "assignee" => Right(SearchSql.userMatch(fr"t.assignee_id", v, ctx.userId))
      case "reporter" => Right(SearchSql.userMatch(fr"t.reporter_id", v, ctx.userId))
      case "watcher"  => Right(fr"exists(select 1 from task_watcher w where w.task_id = t.id and" ++ SearchSql.userMatch(fr"w.user_id", v, ctx.userId) ++ fr")")
      case "label" =>
        Right(Sql.or(SearchSql.values(v).map {
          case "none" | "なし" => fr"not exists(select 1 from task_label tl where tl.task_id = t.id)"
          case x => fr"exists(select 1 from task_label tl join label l on l.id = tl.label_id where tl.task_id = t.id and lower(l.name) = lower($x))"
        }))
      case "sprint" =>
        Right(Sql.or(SearchSql.values(v).map {
          case "active" | "current" | "アクティブ" | "現在" => fr"sp.state = 'active'"
          case "backlog" | "none" | "バックログ" | "なし"   => fr"t.sprint_id is null"
          case "planned" | "future" | "計画中"            => fr"sp.state = 'planned'"
          case "closed" | "完了"                          => fr"sp.state = 'closed'"
          case x => uuidOpt(x).fold(fr"sp.name = $x")(id => fr"t.sprint_id = $id")
        }))
      case "priority" =>
        // priority:high、priority>=high（高い以上）
        SearchSql.values(v).traverse(x => Priority.parse(x).toRight(s"優先度は ${Priority.all.mkString(" / ")} です: $x")).map { ps =>
          val ranks = ps.map(Priority.rank)
          def atLeast(r: Int) = Priority.all.filter(p => Priority.rank(p) >= r)
          def atMost(r: Int)  = Priority.all.filter(p => Priority.rank(p) <= r)
          val set = f.op match
            case SearchQuery.Op.Ge => atLeast(ranks.min)
            case SearchQuery.Op.Gt => atLeast(ranks.min + 1)
            case SearchQuery.Op.Le => atMost(ranks.max)
            case SearchQuery.Op.Lt => atMost(ranks.max - 1)
            case _                 => ps
          fr"t.priority = any(${set.toArray})"
        }
      case "type" =>
        SearchSql.values(v).traverse(x => TaskType.parse(x).toRight(s"種類は ${TaskType.all.mkString(" / ")} です: $x")).map(ts => fr"t.type = any(${ts.toArray})")
      case "due"      => SearchSql.dateCol(fr"t.due_date", f.op, v, ctx.today)
      case "start"    => SearchSql.dateCol(fr"t.start_date", f.op, v, ctx.today)
      case "created"  => SearchSql.dateCol(fr"(t.created_at at time zone 'Asia/Tokyo')", f.op, v, ctx.today)
      case "updated"  => SearchSql.dateCol(fr"(t.updated_at at time zone 'Asia/Tokyo')", f.op, v, ctx.today)
      case "resolved" => SearchSql.dateCol(fr"(t.resolved_at at time zone 'Asia/Tokyo')", f.op, v, ctx.today)
      case "is" =>
        SearchSql.values(v).traverse {
          case "open" | "未完了"           => Right(fr"s.category <> 'done'")
          case "done" | "closed" | "完了"  => Right(fr"s.category = 'done'")
          case "overdue" | "期限切れ"      => Right(fr"t.due_date < ${ctx.today} and s.category <> 'done'")
          case "unassigned" | "未割り当て" => Right(fr"t.assignee_id is null")
          case "subtask" | "サブタスク"     => Right(fr"t.parent_id is not null")
          case "top" | "parent"           => Right(fr"t.parent_id is null")
          case "mine" | "自分"             => Right(fr"t.assignee_id = ${ctx.userId}")
          case "watching" | "ウォッチ中"   => Right(fr"exists(select 1 from task_watcher w where w.task_id = t.id and w.user_id = ${ctx.userId})")
          case x                           => Left(s"is: に使えない値です: $x")
        }.map(Sql.and)
      case "has" =>
        SearchSql.values(v).traverse {
          case "attachment" | "attachments" | "添付" => Right(fr"exists(select 1 from attachment x where x.task_id = t.id)")
          case "subtasks" | "subtask" | "サブタスク" => Right(fr"exists(select 1 from task x where x.parent_id = t.id and x.deleted_at is null)")
          case "commits" | "commit" | "コミット"     => Right(fr"exists(select 1 from task_commit x where x.task_id = t.id)")
          case "comments" | "comment" | "コメント"   => Right(fr"exists(select 1 from comment x where x.task_id = t.id and x.deleted_at is null)")
          case "links" | "link" | "リンク"           => Right(fr"exists(select 1 from task_link x where x.task_id = t.id or x.target_task_id = t.id)")
          case "due" | "期日"                         => Right(fr"t.due_date is not null")
          case "description" | "説明"                => Right(fr"t.description_text <> ''")
          case "worklog" | "作業時間"                 => Right(fr"exists(select 1 from worklog x where x.task_id = t.id)")
          case x                                     => Left(s"has: に使えない値です: $x")
        }.map(Sql.and)
      case "parent" =>
        Right(Sql.or(SearchSql.values(v).map(x => fr"t.parent_id in (select id from task where key = ${x.toUpperCase})")))
      case "key" => Right(SearchSql.key(v))
      case "field" =>
        // field:名前=値（値を省くと「値がある」）
        val (name, value) = v.split("=", 2) match
          case Array(n, x) => (n.trim, Some(x.trim).filter(_.nonEmpty))
          case Array(n)    => (n.trim, None)
          case _           => (v, None)
        Right(value match
          case None => fr"exists(select 1 from task_field_value fv join custom_field cf on cf.id = fv.field_id where fv.task_id = t.id and cf.name = $name)"
          case Some(x) =>
            fr"""exists(select 1 from task_field_value fv join custom_field cf on cf.id = fv.field_id where fv.task_id = t.id and cf.name = $name
                 and ((jsonb_typeof(fv.value) = 'string' and fv.value #>> '{}' ilike ${"%" + x + "%"}) or jsonb_exists(fv.value, $x)))""")
      case "commented" => Right(SearchSql.commented(v, ctx.userId, publicOnly = false))
      case "comment"   => Right(SearchSql.comment(v, publicOnly = false))
      case other => Left(s"使えない条件です: $other")
    res.map(fr => if f.negate then fr"not (" ++ fr ++ fr")" else fr)

  private def orderSql(order: List[(String, Boolean)], hasText: Boolean, text: String): Fragment =
    val parts = order.map((k, asc) => (if k == "key" then fr"p.key, t.number" else SearchSql.orderCol(k)) ++ (if asc then fr"asc nulls last" else fr"desc nulls last"))
    val rel = if hasText then List(fr"(t.title &@~ $text) desc") else Nil
    fr"order by" ++ Sql.commas(parts ++ rel ++ List(fr"t.updated_at desc", fr"t.id"))

  /** 検索する。exact で見つかった件数が少なければ、タイプミスを許容した一致（fuzzy）を下に足す。 */
  def search(q: SearchQuery.Query, ctx: Ctx, limit: Int, offset: Int): IO[Either[String, SearchResultDto]] =
    q.filters.traverse(filterSql(_, ctx)) match
      case Left(e) => IO.pure(Left(e))
      case Right(fs) =>
        val text = q.text.map(SearchQuery.phrase).mkString(" ")
        val negs = q.negText.map(w => fr"not (array[t.key, t.title, t.description_text] &@~ ${SearchQuery.phrase(w)})")
        val hasText = q.text.nonEmpty
        def textCond(fuzzy: Boolean): Fragment = SearchSql.textCond(text, fuzzy, publicOnly = false)
        def run(fuzzy: Boolean, exclude: List[UUID], lim: Int, off: Int): ConnectionIO[List[(UUID, String, Option[String], Long)]] =
          val where = List(
            Some(fr"t.deleted_at is null"),
            Some(fr"t.project_id = any(${ctx.projectIds.toArray})"),
            Option.when(hasText)(textCond(fuzzy)),
            Option.when(exclude.nonEmpty)(fr"t.id <> all(${exclude.toArray})")
          ).flatten ++ fs ++ negs
          val snippet =
            if hasText then fr"(pgroonga_snippet_html(t.description_text, pgroonga_query_extract_keywords($text), 160))[1]"
            else fr"null::text"
          (fr"select t.id, p.key," ++ snippet ++ fr", count(*) over () from task t join project p on p.id = t.project_id" ++
            fr"join status s on s.id = t.status_id left join sprint sp on sp.id = t.sprint_id" ++
            Sql.whereAnd(where) ++ orderSql(q.order, hasText, text) ++ fr"limit $lim offset $off")
            .query[(UUID, String, Option[String], Long)].to[List]
        val prog = for
          exact <- run(fuzzy = false, Nil, limit, offset)
          total = exact.headOption.map(_._4.toInt).getOrElse(0)
          fuzzy <-
            if hasText && total < 5 && offset == 0 then run(fuzzy = true, exact.map(_._1), limit - exact.size, 0)
            else List.empty.pure[ConnectionIO]
        yield (exact.map(r => (r._1, r._2, r._3, false)) ++ fuzzy.map(r => (r._1, r._2, r._3, true)), total + fuzzy.headOption.map(_._4.toInt).getOrElse(0))
        tx(prog).attempt.flatMap {
          case Left(e) => skea.observe.Log.println(s"[skea] search failed: ${e.getMessage}").as(Left("検索できませんでした。条件を見直してください"))
          case Right((rows, total)) =>
            tasks.dtos(rows.map(_._1)).map { ds =>
              val m = ds.map(d => d.id -> d).toMap
              Right(SearchResultDto(rows.flatMap(r => m.get(r._1).map(t => SearchHitDto(t, r._2, r._3, r._4))), total, None))
            }
        }

  // ---------------------------------------------------------------- 保存した検索条件

  def savedFilters(userId: UUID): IO[List[SavedFilterDto]] = tx(
    sql"select id, name, query from saved_filter where user_id = $userId order by name".query[SavedFilterDto].to[List]
  )

  def saveFilter(userId: UUID, name: String, query: String): IO[SavedFilterDto] = tx {
    val id = UUID.randomUUID()
    sql"insert into saved_filter (id, user_id, name, query) values ($id, $userId, $name, $query)".update.run.as(SavedFilterDto(id, name, query))
  }

  def deleteFilter(userId: UUID, id: UUID): IO[Unit] = tx(sql"delete from saved_filter where id = $id and user_id = $userId".update.run.void)

  // ---------------------------------------------------------------- ボードの絞り込み条件

  /** プロジェクトで共有しているものと、自分のもの（共有を先に、名前の順）。 */
  def boardFilters(projectId: UUID, userId: UUID): IO[List[BoardFilterDto]] = tx(
    sql"""select id, name, cond, user_id is null, created_by = $userId from board_filter
          where project_id = $projectId and (user_id is null or user_id = $userId) order by user_id is not null, name"""
      .query[(UUID, String, Json, Boolean, Option[Boolean])].to[List]
  ).map(_.map((id, n, c, sh, mine) => BoardFilterDto(id, n, c.as[BoardCond].getOrElse(BoardCond()), sh, mine.getOrElse(false) || !sh)))

  /** 1件（プロジェクト・持ち主（共有なら None）・作った人）。 */
  def boardFilter(id: UUID): IO[Option[(UUID, Option[UUID], Option[UUID])]] =
    tx(sql"select project_id, user_id, created_by from board_filter where id = $id".query[(UUID, Option[UUID], Option[UUID])].option)

  def saveBoardFilter(projectId: UUID, userId: UUID, r: BoardFilterReq): IO[Either[StoreError, BoardFilterDto]] = tx {
    val id = UUID.randomUUID()
    val owner = Option.unless(r.shared)(userId)
    sql"""insert into board_filter (id, project_id, user_id, name, cond, created_by)
          values ($id, $projectId, $owner, ${r.name.trim}, ${r.cond.asJson}, $userId)""".update.run
      .as(BoardFilterDto(id, r.name.trim, r.cond, r.shared, true)).attemptSqlState.map(_.left.map {
        case s if s.value == "23505" => StoreError.Conflict("同じ名前の条件があります")
        case s                       => StoreError.Invalid(s"保存できませんでした（${s.value}）")
      })
  }

  def deleteBoardFilter(id: UUID): IO[Unit] = tx(sql"delete from board_filter where id = $id".update.run.void)

/**
 * 検索の SQL の部品（エージェントの検索と、ヘルプデスクの窓口の検索で共通）。テーブルの別名は t（task）・s（status）。
 * publicOnly なら、コメントは公開のもの（内部コメントを除く）だけを見る（"ユーザ"に内部コメントの中身や有無を漏らさない）。
 */
object SearchSql:

  /** タイプミスを許容する度合い（バイグラムの2文字の語で1文字違いまで）。 */
  val FuzzyRatio = 0.5

  def values(v: String): List[String] = v.split(',').toList.map(_.trim).filter(_.nonEmpty)

  def userMatch(col: Fragment, v: String, me: UUID): Fragment =
    Sql.or(values(v).map {
      case "me" | "自分" | "currentuser()" => col ++ fr"= $me"
      case "none" | "なし" | "empty"       => col ++ fr"is null"
      case x =>
        uuidOpt(x) match
          case Some(id) => col ++ fr"= $id"
          case None => fr"exists(select 1 from app_user u where u.id =" ++ col ++
            fr"and (u.name ilike ${"%" + x + "%"} or lower(u.email) = lower($x) or u.sub = $x))"
    })

  def dateCol(col: Fragment, op: Op, v: String, today: LocalDate): Either[String, Fragment] =
    if v == "none" || v == "なし" then Right(col ++ fr"is null")
    else
      SearchQuery.date(v, today).toRight(s"日付として読めません: $v").map { d =>
        op match
          case Op.Eq => col ++ fr"::date = $d"
          case Op.Lt => col ++ fr"::date < $d"
          case Op.Le => col ++ fr"::date <= $d"
          case Op.Gt => col ++ fr"::date > $d"
          case Op.Ge => col ++ fr"::date >= $d"
      }

  val categoryAlias: Map[String, String] = Map(
    "todo" -> StatusCategory.Todo, "作業前" -> StatusCategory.Todo, "未着手" -> StatusCategory.Todo,
    "in_progress" -> StatusCategory.InProgress, "inprogress" -> StatusCategory.InProgress, "進行中" -> StatusCategory.InProgress,
    "waiting" -> StatusCategory.Waiting, "待機中" -> StatusCategory.Waiting, "保留" -> StatusCategory.Waiting,
    "done" -> StatusCategory.Done, "完了" -> StatusCategory.Done
  )

  def category(v: String): Either[String, Fragment] =
    values(v).traverse(x => if x == "open" then Right("open") else categoryAlias.get(x.toLowerCase).toRight(s"カテゴリは todo / in_progress / waiting / done です: $x"))
      .map(cs => Sql.or(cs.map(c => if c == "open" then fr"s.category <> 'done'" else fr"s.category = $c")))

  def status(v: String): Fragment = Sql.or(values(v).map(x => uuidOpt(x).fold(fr"lower(s.name) = lower($x)")(id => fr"s.id = $id")))

  def key(v: String): Fragment = fr"t.key = any(${values(v).map(_.toUpperCase).toArray})"

  private def commentScope(publicOnly: Boolean): Fragment =
    if publicOnly then fr"c.deleted_at is null and not c.internal" else fr"c.deleted_at is null"

  def commented(v: String, me: UUID, publicOnly: Boolean): Fragment =
    fr"exists(select 1 from comment c where c.task_id = t.id and" ++ commentScope(publicOnly) ++ fr"and" ++ userMatch(fr"c.author_id", v, me) ++ fr")"

  def comment(v: String, publicOnly: Boolean): Fragment =
    fr"exists(select 1 from comment c where c.task_id = t.id and" ++ commentScope(publicOnly) ++ fr"and c.body_text &@~ ${SearchQuery.phrase(v)})"

  /** 全文検索（タスクの key・タイトル・説明と、コメントをそれぞれのインデックスで探す. OR で1つの条件にするとインデックスが使われない）。 */
  def textCond(text: String, fuzzy: Boolean, publicOnly: Boolean): Fragment =
    val ratio = if fuzzy then FuzzyRatio else 0.0
    fr"""t.id in (select x.id from task x where array[x.key, x.title, x.description_text] &@~
                   pgroonga_condition($text, index_name => 'task_search_idx', fuzzy_max_distance_ratio => $ratio::real)
                 union select c.task_id from comment c where""" ++ commentScope(publicOnly) ++ fr"""and c.body_text &@~
                   pgroonga_condition($text, index_name => 'comment_search_idx', fuzzy_max_distance_ratio => $ratio::real))"""

  def orderCol(k: String): Fragment = k match
    case "due"      => fr"t.due_date"
    case "start"    => fr"t.start_date"
    case "created"  => fr"t.created_at"
    case "updated"  => fr"t.updated_at"
    case "resolved" => fr"t.resolved_at"
    case "key"      => fr"t.project_id, t.number"
    case "rank"     => fr"""t.rank collate "C""""
    case "title"    => fr"t.title"
    case "status"   => fr"s.position"
    case "priority" => fr"array_position(array['lowest','low','medium','high','highest'], t.priority)"
    case "type"     => fr"t.type"
    case _          => fr"(select name from app_user where id = t.assignee_id)"
