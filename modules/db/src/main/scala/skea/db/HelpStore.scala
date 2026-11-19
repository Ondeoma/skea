package skea.db

import sbirds.observe.Log

import skea.core.ErrorCode

import sbirds.shared.AppError

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.postgres.circe.jsonb.implicits.*
import skea.core.SearchQuery
import skea.core.SearchQuery.Filter
import skea.shared.*

import java.time.{Instant, LocalDate}
import java.util.UUID

/** 窓口（/help/KEY）の表示とゲスト登録の判定に使う、プロジェクトのヘルプデスクの設定。 */
final case class HelpdeskProject(id: UUID,
                                 key: String,
                                 name: String,
                                 description: String,
                                 archived: Boolean,
                                 helpdesk: Boolean,
                                 guestSignup: Boolean,
                                 guestDomains: List[String],
                                 doneHideDays: Int) {

  /** このメールアドレスでゲスト登録できるか（ドメインの指定がなければ誰でもよい）。 */
  def domainAllowed(email: Option[String]): Boolean =
    guestDomains.isEmpty ||
      email.exists(e => guestDomains.exists(d => e.toLowerCase.endsWith("@" + d.toLowerCase.stripPrefix("@"))))
  def acceptsGuests: Boolean = helpdesk && guestSignup && !archived
}

/**
 * ヘルプデスク: "ユーザ"（requester）のグループ（親子の階層）、タスクの共有、"ユーザ"に見えるタスクの判定。
 *
 * "ユーザ"に見えるタスクは、自分が報告者のものと、自分か自分のグループに共有されたもの。
 * グループに共有したタスクは、そのグループと子孫のグループのメンバーに見える（親に共有→子にも見える）。
 */
final class HelpStore(xa: Transactor[IO]) extends TxStore(xa) {

  // ---------------------------------------------------------------- 見えるタスクの判定

  /** 自分が直接入っているグループと、その祖先のグループ（共有されていれば見える）。 */
  private def myGroups(userId: UUID): Fragment =
    fr"""with recursive mine(id) as (
           select group_id from requester_group_member where user_id = $userId
           union select g.parent_id from requester_group g join mine m on g.id = m.id where g.parent_id is not null
         ) select id from mine"""

  /** タスク t（別名）が userId に"ユーザ"として見える条件。 */
  private def visibleTo(userId: UUID): Fragment =
    fr"""(t.reporter_id = $userId or exists(select 1 from task_share ts where ts.task_id = t.id
           and (ts.user_id = $userId or ts.group_id in (""" ++ myGroups(userId) ++ fr"))))"

  def canSee(taskId: UUID, userId: UUID): IO[Boolean] =
    tx((fr"select exists(select 1 from task t where t.id = $taskId and t.deleted_at is null and" ++ visibleTo(userId) ++
      fr")").query[Boolean].unique)

  private val taskCols =
    fr"""t.id, t.key, t.project_id, t.title, s.name, s.category, s.color, r.id, r.name, r.avatar_url, a.id, a.name, a.avatar_url,
         t.created_at, t.updated_at"""
  private val taskFrom =
    fr"""from task t join status s on s.id = t.status_id left join app_user r on r.id = t.reporter_id left join app_user a on a.id = t.assignee_id"""

  private type TaskRow =
    (UUID, String, UUID, String, String, String, String, Option[UserBrief], Option[UserBrief], Instant, Instant)

  private def taskDto(userId: UUID)(r: TaskRow): HelpTaskDto =
    HelpTaskDto(r._1, r._2, r._3, r._4, r._5, r._6, r._7, r._8, r._9, r._10, r._11, r._8.exists(_.id == userId))

  /** プロジェクトのタスクのうち userId に見えるもの（新しく更新された順）。done が false なら完了のものを除く。 */
  def visibleTasks(projectId: UUID, userId: UUID, done: Boolean, limit: Int = 500): IO[List[HelpTaskDto]] =
    tx((fr"select" ++ taskCols ++ taskFrom ++ fr"where t.project_id = $projectId and t.deleted_at is null and" ++
      visibleTo(userId) ++ (if done then Fragment.empty else fr"and s.category <> 'done'") ++
      fr"order by t.updated_at desc limit $limit").query[TaskRow].map(taskDto(userId)).to[List])

  def task(taskId: UUID, userId: UUID): IO[Option[HelpTaskDto]] = tx((fr"select" ++ taskCols ++ taskFrom ++
    fr"where t.id = $taskId and t.deleted_at is null").query[TaskRow].map(taskDto(userId)).option)

  // ---------------------------------------------------------------- 送信前の添付（一時置き場）

  def addPending(id: UUID,
                 userId: UUID,
                 projectId: UUID,
                 key: String,
                 name: String,
                 size: Long,
                 ct: String,
                 sha: String): IO[Unit] = tx(
    sql"""insert into pending_upload (id, user_id, project_id, s3_key, file_name, size, content_type, sha256)
          values ($id, $userId, $projectId, $key, $name, $size, $ct, $sha)""".update.run.void
  )

  /** 本人の一時置き場のファイル（s3_key, 名前, 種類）。 */
  def pending(id: UUID, userId: UUID): IO[Option[(String, String, String)]] =
    tx(sql"select s3_key, file_name, content_type from pending_upload where id = $id and user_id = $userId".query[(
      String,
      String,
      String
    )].option)

  /** 本人の一時置き場のうち ids のもの（このプロジェクトに置いたもの）。 */
  def pendingIds(ids: List[UUID], userId: UUID, projectId: UUID): IO[List[UUID]] = tx(
    sql"select id from pending_upload where id = any(${ids.toArray}) and user_id = $userId and project_id = $projectId".query[
      UUID
    ].to[List]
  )

  /** 一時置き場から問い合わせの添付に移す（id はそのまま。S3 のファイルもそのまま使う）。 */
  def attachPending(ids: List[UUID], userId: UUID, taskId: UUID): IO[Int] = tx(
    sql"""insert into attachment (id, task_id, s3_key, file_name, size, content_type, sha256, created_by)
          select id, $taskId, s3_key, file_name, size, content_type, sha256, user_id from pending_upload
          where id = any(${ids.toArray}) and user_id = $userId""".update.run <*
      sql"delete from pending_upload where id = any(${ids.toArray}) and user_id = $userId".update.run
  )

  /** 本人の一時置き場のうち、before より前のもの（使われなかったもの）を消し、S3 のキーを返す。 */
  def purgePending(userId: UUID, before: Instant): IO[List[String]] =
    tx(sql"delete from pending_upload where user_id = $userId and created_at < $before returning s3_key".query[
      String
    ].to[List])

  // ---------------------------------------------------------------- 問い合わせのフォーム

  /** 窓口のフォームに出す項目の、このタスクの値（フォームの並び順）。 */
  def formValues(taskId: UUID): IO[List[HelpFieldValueDto]] = tx(
    sql"""select f.name, v.value from task_field_value v join custom_field f on f.id = v.field_id
          where v.task_id = $taskId and f.help_form order by f.position, f.name""".query[HelpFieldValueDto].to[List]
  )

  // ---------------------------------------------------------------- 検索

  /** 窓口の検索で使える並び順。 */
  private val orderKeys = Set("created", "updated", "resolved", "key", "title", "status", "assignee")

  /**
   * "ユーザ"に見せてよい条件だけを SQL にする（ラベル・優先度・ウォッチャー・作業時間などエージェント向けの項目は使えない）。
   * コメントは公開のものだけを見る。
   */
  private def filterSql(f: Filter, me: UUID, today: LocalDate): Either[String, Fragment] = {
    val v = f.value
    val res: Either[String, Fragment] = f.key match {
      case "status" => Right(SearchSql.status(v))
      case "category" => SearchSql.category(v)
      case "assignee" => Right(SearchSql.userMatch(fr"t.assignee_id", v, me))
      case "reporter" => Right(SearchSql.userMatch(fr"t.reporter_id", v, me))
      case "created" => SearchSql.dateCol(fr"(t.created_at at time zone 'Asia/Tokyo')", f.op, v, today)
      case "updated" => SearchSql.dateCol(fr"(t.updated_at at time zone 'Asia/Tokyo')", f.op, v, today)
      case "resolved" => SearchSql.dateCol(fr"(t.resolved_at at time zone 'Asia/Tokyo')", f.op, v, today)
      case "key" => Right(SearchSql.key(v))
      case "comment" => Right(SearchSql.comment(v, publicOnly = true))
      case "commented" => Right(SearchSql.commented(v, me, publicOnly = true))
      case "is" =>
        SearchSql.values(v).traverse {
          case "open" | "未完了" => Right(fr"s.category <> 'done'")
          case "done" | "closed" | "完了" => Right(fr"s.category = 'done'")
          case "mine" | "自分" => Right(fr"t.reporter_id = $me")
          case "shared" | "共有" => Right(fr"t.reporter_id is distinct from $me")
          case "watching" | "通知" =>
            Right(fr"exists(select 1 from task_watcher w where w.task_id = t.id and w.user_id = $me)")
          case "unassigned" | "未割り当て" => Right(fr"t.assignee_id is null")
          case x => Left(s"is: に使えない値です: $x（open・done・mine・shared・watching・unassigned）")
        }.map(Sql.and)
      case "has" =>
        SearchSql.values(v).traverse {
          case "attachment" | "attachments" | "添付" =>
            Right(fr"exists(select 1 from attachment x where x.task_id = t.id)")
          case "comments" | "comment" | "コメント" | "返信" =>
            Right(
              fr"exists(select 1 from comment c where c.task_id = t.id and c.deleted_at is null and not c.internal)"
            )
          case "description" | "説明" => Right(fr"t.description_text <> ''")
          case x => Left(s"has: に使えない値です: $x（attachment・comments・description）")
        }.map(Sql.and)
      case other => Left(s"窓口の検索では「$other」は使えません")
    }
    res.map(x => if f.negate then fr"not (" ++ x ++ fr")" else x)
  }

  /**
   * 窓口の検索（絞り込み構文 + 全文検索）。対象は userId に見えるタスク（エージェントは projectId のすべて）だけ。
   * 一致が少なければ、タイプミスを許した一致を下に足す。
   */
  def search(projectId: UUID,
             userId: UUID,
             agent: Boolean,
             q: SearchQuery.Query,
             today: LocalDate,
             limit: Int,
             offset: Int)
    : IO[Either[String, HelpSearchResultDto]] = {
    val badOrder = q.order.map(_._1).find(k => !orderKeys(k))
    (q.filters.traverse(filterSql(_, userId, today)), badOrder) match {
      case (Left(e), _) => IO.pure(Left(e))
      case (_, Some(k)) => IO.pure(Left(s"窓口の検索では「$k」で並べられません"))
      case (Right(fs), None) =>
        val text = q.text.map(SearchQuery.phrase).mkString(" ")
        val hasText = q.text.nonEmpty
        // 除外する語は、件名・説明と公開の返信のどれにも含まないもの
        val negs = q.negText.map { w =>
          val ph = SearchQuery.phrase(w)
          fr"not (array[t.key, t.title, t.description_text] &@~ $ph)" ++
            fr"and not exists(select 1 from comment c where c.task_id = t.id and c.deleted_at is null and not c.internal and c.body_text &@~ $ph)"
        }
        val order = {
          val parts =
            q.order.map((k, asc) => SearchSql.orderCol(k) ++ (if asc then fr"asc nulls last" else fr"desc nulls last"))
          val rel = if hasText then List(fr"(t.title &@~ $text) desc") else Nil
          fr"order by" ++ Sql.commas(parts ++ rel ++ List(fr"t.updated_at desc", fr"t.id"))
        }
        def run(fuzzy: Boolean,
                exclude: List[UUID],
                lim: Int,
                off: Int): ConnectionIO[List[(TaskRow, Option[String], Long)]] = {
          val where = List(
            Some(fr"t.project_id = $projectId and t.deleted_at is null"),
            Option.when(!agent)(visibleTo(userId)),
            Option.when(hasText)(SearchSql.textCond(text, fuzzy, publicOnly = true)),
            Option.when(exclude.nonEmpty)(fr"t.id <> all(${exclude.toArray})")
          ).flatten ++ fs ++ negs
          val snippet =
            if hasText then
              fr"(pgroonga_snippet_html(t.description_text, pgroonga_query_extract_keywords($text), 160))[1]"
            else fr"null::text"
          (fr"select" ++ taskCols ++ fr"," ++ snippet ++ fr", count(*) over ()" ++ taskFrom ++ Sql.whereAnd(where) ++
            order ++ fr"limit $lim offset $off")
            .query[(TaskRow, Option[String], Long)].to[List]
        }
        val prog = for {
          exact <- run(fuzzy = false, Nil, limit, offset)
          total = exact.headOption.map(_._3.toInt).getOrElse(0)
          fuzzy <- if hasText && total < 5 && offset == 0 then
            run(fuzzy = true, exact.map(_._1._1), limit - exact.size, 0)
          else List.empty.pure[ConnectionIO]
        } yield HelpSearchResultDto(
          exact.map(r => HelpHitDto(taskDto(userId)(r._1), r._2, fuzzy = false)) ++
            fuzzy.map(r => HelpHitDto(taskDto(userId)(r._1), r._2, fuzzy = true)),
          total + fuzzy.headOption.map(_._3.toInt).getOrElse(0),
          None
        )
        tx(prog).attempt.flatMap {
          case Left(e) =>
            Log.report(ErrorCode.HelpSearchFailed, e.getMessage, e).as(Left("検索できませんでした。条件を見直してください"))
          case Right(r) => IO.pure(Right(r))
        }
    }
  }

  // ---------------------------------------------------------------- 設定

  private val projSelect =
    fr"select id, key, name, description, archived, helpdesk, guest_signup, guest_domains, done_hide_days from project"

  def project(id: UUID): IO[Option[HelpdeskProject]] =
    tx((projSelect ++ fr"where id = $id").query[HelpdeskProject].option)

  def projectByKey(key: String): IO[Option[HelpdeskProject]] =
    tx((projSelect ++ fr"where key = ${key.trim.toUpperCase}").query[HelpdeskProject].option)

  /** ヘルプデスクのプロジェクトのうち ids に含まれるもの（窓口の一覧）。 */
  def helpdeskProjects(ids: List[UUID]): IO[List[HelpdeskProject]] = tx(
    (projSelect ++ fr"where helpdesk and not archived and id = any(${ids.toArray}) order by name").query[
      HelpdeskProject
    ].to[List]
  )

  /**
   * 設定を変える。ヘルプデスクにしたら、すべてのタスクをアクティブスプリント（なければ作る）に移す
   * （以後は DB のトリガー task_helpdesk_sprint が、作成・更新のたびにアクティブスプリントに置く）。
   */
  def updateSettings(projectId: UUID, r: HelpdeskSettingsReq): IO[Unit] = tx(
    sql"""update project set helpdesk = ${r.helpdesk}, guest_signup = ${r.guestSignup},
          guest_domains = ${r.guestDomains.map(
        _.trim.toLowerCase.stripPrefix("@")
      ).filter(_.nonEmpty).distinct.toArray},
          done_hide_days = coalesce(${r.doneHideDays}, done_hide_days)
          where id = $projectId""".update.run *>
      sql"""update task set sprint_id = helpdesk_sprint($projectId)
            where project_id = $projectId and deleted_at is null""".update.run.whenA(r.helpdesk)
  )

  /** "ユーザ"として参加している人。 */
  def requesters(projectId: UUID): IO[List[UserBrief]] = tx(requestersC(projectId))

  private def requestersC(projectId: UUID): ConnectionIO[List[UserBrief]] =
    sql"""select u.id, u.name, u.avatar_url from project_member m join app_user u on u.id = m.user_id
          where m.project_id = $projectId and m.role = 'requester' and not u.disabled order by u.name""".query[
      UserBrief
    ].to[List]

  /** ゲスト登録: まだメンバーでなければ"ユーザ"として加える（すでにエージェントならそのまま）。加えたら true。 */
  def join(projectId: UUID, userId: UUID): IO[Boolean] = tx(
    sql"""insert into project_member (project_id, user_id, role) values ($projectId, $userId, 'requester')
          on conflict (project_id, user_id) do nothing""".update.run.map(_ > 0)
  )

  def settings(projectId: UUID): IO[Option[HelpdeskSettingsDto]] =
    (project(projectId), groups(projectId), requesters(projectId)).mapN((p, gs, rs) =>
      p.map(x => HelpdeskSettingsDto(x.helpdesk, x.guestSignup, x.guestDomains, gs, rs, x.doneHideDays))
    )

  // ---------------------------------------------------------------- グループ

  def groups(projectId: UUID): IO[List[RequesterGroupDto]] = tx(groupsC(projectId))

  private def groupsC(projectId: UUID): ConnectionIO[List[RequesterGroupDto]] =
    for {
      gs <- sql"select id, parent_id, name, domain from requester_group where project_id = $projectId order by name"
        .query[(UUID, Option[UUID], String, Option[String])].to[List]
      ms <-
        sql"""select m.group_id, u.id, u.name, u.avatar_url from requester_group_member m join requester_group g on g.id = m.group_id
                  join app_user u on u.id = m.user_id where g.project_id = $projectId and not u.disabled order by u.name"""
          .query[(UUID, UserBrief)].to[List]
    } yield gs.map((id, p, n, d) => RequesterGroupDto(id, p, n, ms.filter(_._1 == id).map(_._2), d))

  /** 親がこのプロジェクトのグループで、自分自身や子孫を親にしない（循環しない）ことを確かめる。 */
  private def parentOkC(projectId: UUID, id: Option[UUID], parent: Option[UUID]): ConnectionIO[Either[AppError, Unit]] =
    parent match {
      case None => Right(()).pure[ConnectionIO]
      case Some(p) =>
        sql"""with recursive up(id) as (select $p::uuid union select g.parent_id from requester_group g join up on g.id = up.id where g.parent_id is not null)
              select (select project_id from requester_group where id = $p), array_agg(id) from up"""
          .query[(Option[UUID], Option[List[UUID]])].unique.map {
            (pp, chain) =>
              if !pp.contains(projectId) then Left(AppError(ErrorCode.HelpGroupParentNotFound))
              else if id.exists(i => chain.getOrElse(Nil).contains(i)) then
                Left(AppError(ErrorCode.HelpGroupParentCycle))
              else Right(())
          }
    }

  private def uniqueName[A](q: ConnectionIO[A], name: String): ConnectionIO[Either[AppError, A]] =
    q.attemptSqlState.map(_.left.map(_ => AppError(ErrorCode.HelpGroupNameTaken, s"グループ「$name」はすでにあります")))

  private def domainOf(r: RequesterGroupReq): Option[String] =
    r.domain.map(RequesterGroups.normalizeDomain).filter(_.nonEmpty)

  def createGroup(projectId: UUID, r: RequesterGroupReq): IO[Either[AppError, UUID]] = tx(
    parentOkC(projectId, None, r.parentId).flatMap {
      case Left(e) => fail(e)
      case Right(_) =>
        val id = UUID.randomUUID()
        uniqueName(
          sql"""insert into requester_group (id, project_id, parent_id, name, domain)
                       values ($id, $projectId, ${r.parentId}, ${r.name.trim}, ${domainOf(r)})"""
            .update.run.as(id),
          r.name.trim
        )
    }
  )

  def updateGroup(projectId: UUID, id: UUID, r: RequesterGroupReq): IO[Either[AppError, Unit]] = tx(
    parentOkC(projectId, Some(id), r.parentId).flatMap {
      case Left(e) => fail(e)
      case Right(_) =>
        uniqueName(
          sql"""update requester_group set name = ${r.name.trim}, parent_id = ${r.parentId}, domain = ${domainOf(r)}
                       where id = $id and project_id = $projectId"""
            .update.run,
          r.name.trim
        ).map(_.flatMap(n => Either.cond(n > 0, (), AppError(ErrorCode.HelpGroupNotFound))))
    }
  )

  /** グループを消す（子のグループも消える）。 */
  def deleteGroup(projectId: UUID, id: UUID): IO[Unit] =
    tx(sql"delete from requester_group where id = $id and project_id = $projectId".update.run.void)

  /** グループにメンバーを加える（このプロジェクトの"ユーザ"だけ）。 */
  def addGroupMember(projectId: UUID, groupId: UUID, userId: UUID): IO[Either[AppError, Unit]] = tx(
    sql"""select exists(select 1 from requester_group where id = $groupId and project_id = $projectId),
                 exists(select 1 from project_member where project_id = $projectId and user_id = $userId and role = 'requester')"""
      .query[(Boolean, Boolean)].unique.flatMap {
        case (false, _) => fail(AppError(ErrorCode.HelpGroupNotFound))
        case (_, false) => fail(AppError(ErrorCode.HelpGroupMemberNotRequester))
        case _ =>
          sql"insert into requester_group_member (group_id, user_id) values ($groupId, $userId) on conflict do nothing".update.run.as(
            Right(())
          )
      }
  )

  def removeGroupMember(projectId: UUID, groupId: UUID, userId: UUID): IO[Unit] = tx(
    sql"""delete from requester_group_member m using requester_group g
          where m.group_id = g.id and g.id = $groupId and g.project_id = $projectId and m.user_id = $userId""".update.run.void
  )

  // ---------------------------------------------------------------- 共有

  def shares(taskId: UUID, projectId: UUID): IO[TaskSharesDto] = tx(
    for {
      gids <- sql"select group_id from task_share where task_id = $taskId and group_id is not null".query[UUID].to[List]
      gs <- groupsC(projectId)
      us <- sql"""select u.id, u.name, u.avatar_url from task_share s join app_user u on u.id = s.user_id
                    where s.task_id = $taskId order by u.name""".query[UserBrief].to[List]
    } yield TaskSharesDto(gs.filter(g => gids.contains(g.id)), us)
  )

  /** 共有先の名前（"ユーザ"の画面に出す）。 */
  def shareNames(taskId: UUID): IO[List[String]] = tx(
    sql"""select g.name from task_share s join requester_group g on g.id = s.group_id where s.task_id = $taskId
          union all select u.name from task_share s join app_user u on u.id = s.user_id where s.task_id = $taskId""".query[
      String
    ].to[List]
  )

  /** 共有で見える人（グループは子孫のグループのメンバーも）。 */
  private def shareAudienceC(taskId: UUID): ConnectionIO[Set[UUID]] =
    sql"""with recursive down(id) as (
             select group_id from task_share where task_id = $taskId and group_id is not null
             union select g.id from requester_group g join down d on g.parent_id = d.id
           )
           select m.user_id from requester_group_member m where m.group_id in (select id from down)
           union select user_id from task_share where task_id = $taskId and user_id is not null""".query[
      UUID
    ].to[List].map(_.toSet)

  /** グループの共有と、共有した人（窓口で"ユーザ"が自分の共有だけを外せるようにする）。 */
  def groupShares(taskId: UUID): IO[List[(UUID, Option[UUID])]] =
    tx(sql"select group_id, created_by from task_share where task_id = $taskId and group_id is not null".query[(
      UUID,
      Option[UUID]
    )].to[List])

  /**
   * 共有先を置き換える（グループ・個人はこのプロジェクトのものだけ）。新しく共有で見えるようになった人を返す。
   * 変わらない共有はそのまま残す（共有した人も変えない）。watch なら、新しく見えるようになった人をウォッチャーにも登録する。
   */
  def setShares(taskId: UUID,
                projectId: UUID,
                groupIds: List[UUID],
                userIds: List[UUID],
                by: UUID,
                watch: Boolean = true)
    : IO[Either[AppError, Set[UUID]]] = tx(
    for {
      okG <-
        sql"select id from requester_group where project_id = $projectId and id = any(${groupIds.distinct.toArray})".query[
          UUID
        ].to[List]
      okU <- sql"""select user_id from project_member where project_id = $projectId and role = 'requester'
                   and user_id = any(${userIds.distinct.toArray})""".query[UUID].to[List]
      r <- if okG.size != groupIds.distinct.size then fail(AppError(ErrorCode.HelpShareGroupNotFound))
      else if okU.size != userIds.distinct.size then fail(AppError(ErrorCode.HelpShareUserNotRequester))
      else
        for {
          before <- shareAudienceC(taskId)
          _ <- sql"""delete from task_share where task_id = $taskId and (
                            (group_id is not null and group_id <> all(${okG.toArray})) or (user_id is not null and user_id <> all(${okU.toArray})))"""
            .update.run
          _ <- okG.traverse_(g =>
            sql"insert into task_share (task_id, group_id, created_by) values ($taskId, $g, $by) on conflict do nothing".update.run
          )
          _ <- okU.traverse_(u =>
            sql"insert into task_share (task_id, user_id, created_by) values ($taskId, $u, $by) on conflict do nothing".update.run
          )
          after <- shareAudienceC(taskId)
          added = after -- before
          _ <- added.toList.traverse_(u =>
            sql"insert into task_watcher (task_id, user_id) values ($taskId, $u) on conflict do nothing".update.run
          )
            .whenA(watch)
        } yield Right(added)
    } yield r
  )
}
