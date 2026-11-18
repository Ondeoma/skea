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
import skea.shared.*

import java.time.{Instant, LocalDate}
import java.util.UUID

/** プロジェクトと、その設定（メンバー・ステータス・ラベル・スプリント・カスタムフィールド）。 */
final class ProjectStore(xa: Transactor[IO]) extends TxStore(xa):

  // ---------------------------------------------------------------- 共通

  /** プロジェクトの設定（ステータス・カスタムフィールド・スプリント）の並び順を ids の順にする。 */
  private def reorderC(table: String, projectId: UUID, ids: List[UUID]): ConnectionIO[Unit] =
    ids.zipWithIndex.traverse_((id, i) =>
      (fr"update" ++ Fragment.const(table) ++ fr"set position = $i where id = $id and project_id = $projectId").update.run)

  /** 名前が一意の設定を更新する（名前の重複は Conflict、対象がなければ NotFound）。 */
  private def updateUniqueC(q: Fragment, what: String, name: String): ConnectionIO[Either[StoreError, Unit]] =
    q.update.run.attemptSql.map {
      case Left(_)  => Left(StoreError.Conflict(s"$what「$name」はすでにあります"))
      case Right(0) => Left(StoreError.NotFound(what))
      case Right(_) => Right(())
    }

  // ---------------------------------------------------------------- 権限

  /** 利用者のプロジェクトでの実効権限（APIキーのスコープ・対象プロジェクトも反映）。見えなければ None。 */
  def roleC(projectId: UUID, s: AuthSession): ConnectionIO[Option[Role]] =
    if !s.allowsProject(projectId) then none[Role].pure[ConnectionIO]
    else
      val gids = s.groups.map(_.id).toArray
      for
        exists <- sql"select exists(select 1 from project where id = $projectId)".query[Boolean].unique
        ur <- sql"select role from project_member where project_id = $projectId and user_id = ${s.user.id}".query[String].option
        gr <- sql"select role from project_group_member where project_id = $projectId and group_id = any($gids)".query[String].to[List]
      yield
        if !exists then None
        else Access.effective(s.isAdmin, ur.flatMap(Role.parse), gr.flatMap(Role.parse)).map(Access.limit(s, _))

  def role(projectId: UUID, s: AuthSession): IO[Option[Role]] = tx(roleC(projectId, s))

  /** 見えるプロジェクトの ID と権限（エージェントとして。ヘルプデスクの"ユーザ"として参加しているものは含めない）。 */
  def visibleC(s: AuthSession): ConnectionIO[Map[UUID, Role]] = membershipsC(s).map(_.filter(_._2.isAgent))

  def visible(s: AuthSession): IO[Map[UUID, Role]] = tx(visibleC(s))

  /** 参加しているプロジェクトの ID と権限（ヘルプデスクの"ユーザ"を含む）。 */
  def memberships(s: AuthSession): IO[Map[UUID, Role]] = tx(membershipsC(s))

  def membershipsC(s: AuthSession): ConnectionIO[Map[UUID, Role]] =
    val gids = s.groups.map(_.id).toArray
    val q =
      if s.isAdmin then sql"select id, 'admin' from project"
      else
        sql"""select project_id, role from project_member where user_id = ${s.user.id}
              union all select project_id, role from project_group_member where group_id = any($gids)"""
    q.query[(UUID, String)].to[List].map { rows =>
      rows.groupMapReduce(_._1)(r => Role.parse(r._2).getOrElse(Role.Requester))((a, b) => if a >= b then a else b)
        .filter((id, _) => s.allowsProject(id))
        .map((id, r) => id -> Access.limit(s, r))
    }

  // ---------------------------------------------------------------- プロジェクト

  private val projCols =
    fr"""p.id, p.key, p.name, p.description, l.id, l.name, l.avatar_url, p.archived, p.created_at, p.helpdesk
         from project p left join app_user l on l.id = p.lead_id"""

  private type ProjRow = (UUID, String, String, String, Option[UserBrief], Boolean, Instant, Boolean)

  def list(s: AuthSession, includeArchived: Boolean): IO[List[ProjectDto]] = tx {
    for
      roles <- visibleC(s)
      rows <- (fr"select" ++ projCols ++ fr"where p.id = any(${roles.keys.toArray})" ++
                (if includeArchived then Fragment.empty else fr"and not p.archived") ++ fr"""order by p.position collate "C", p.name""")
                .query[ProjRow].to[List]
    yield rows.map(r => ProjectDto(r._1, r._2, r._3, r._4, r._5, r._6, roles(r._1).asString, r._7, r._8))
  }

  def find(id: UUID): IO[Option[Project]] = tx(findC(id))

  def findC(id: UUID): ConnectionIO[Option[Project]] =
    sql"select id, key, name, description, lead_id, archived, created_at from project where id = $id".query[Project].option

  def findByKey(key: String): IO[Option[Project]] = tx(
    sql"select id, key, name, description, lead_id, archived, created_at from project where key = ${key.trim.toUpperCase}".query[Project].option
  )

  def dto(id: UUID, role: Role): IO[Option[ProjectDto]] = tx(dtoC(id, role))

  def dtoC(id: UUID, role: Role): ConnectionIO[Option[ProjectDto]] =
    (fr"select" ++ projCols ++ fr"where p.id = $id").query[ProjRow].option
      .map(_.map(r => ProjectDto(r._1, r._2, r._3, r._4, r._5, r._6, role.asString, r._7, r._8)))

  /** 作成者を管理者のメンバーにし、既定のステータス（作業前・進行中・待機中・完了）を作る。 */
  def create(key: String, name: String, description: String, creator: UUID): IO[Either[StoreError, UUID]] = tx {
    sql"select exists(select 1 from project where key = $key)".query[Boolean].unique.flatMap { dup =>
      if dup then fail(StoreError.Conflict(s"プロジェクトキー $key は使われています"))
      else
        val id = UUID.randomUUID()
        for
          last <- sql"""select max(position collate "C") from project""".query[Option[String]].unique
          _ <- sql"""insert into project (id, key, name, description, lead_id, position)
                     values ($id, $key, $name, $description, $creator, ${FractionalIndex.after(last)})""".update.run
          _ <- sql"insert into project_member (project_id, user_id, role) values ($id, $creator, 'admin')".update.run
          _ <- List(
                 ("作業前", StatusCategory.Todo, "#64748b"),
                 ("進行中", StatusCategory.InProgress, "#2563eb"),
                 ("待機中", StatusCategory.Waiting, "#d97706"),
                 ("完了", StatusCategory.Done, "#16a34a")
               ).zipWithIndex.traverse_ { case ((n, c, col), i) =>
                 sql"insert into status (id, project_id, name, category, color, position) values (${UUID.randomUUID()}, $id, $n, $c, $col, $i)".update.run
               }
        yield Right(id)
    }
  }

  def update(id: UUID, r: UpdateProjectReq): IO[Unit] = tx {
    val sets = List(
      r.name.map(_.trim).filter(_.nonEmpty).map(v => fr"name = $v"),
      r.description.map(v => fr"description = $v"),
      r.leadId.map(v => fr"lead_id = $v"),
      r.archived.map(v => fr"archived = $v")
    ).flatten
    if sets.isEmpty then ().pure[ConnectionIO] else (fr"update project" ++ Sql.set(sets) ++ fr"where id = $id").update.run.void
  }

  /** ヘルプデスクなら、ボードで完了したタスクを出す日数。ヘルプデスクでなければ None。 */
  def helpdeskDoneDays(id: UUID): IO[Option[Int]] = tx(
    sql"select done_hide_days from project where id = $id and helpdesk".query[Int].option
  )

  def delete(id: UUID): IO[Unit] = tx(sql"delete from project where id = $id".update.run.void)

  def reorder(ids: List[UUID]): IO[Unit] = tx(
    ids.zipWithIndex.traverse_((id, i) => sql"update project set position = ${f"a$i%05d"} where id = $id".update.run)
  )

  // ---------------------------------------------------------------- メンバー

  def members(projectId: UUID): IO[List[MemberDto]] = tx(membersC(projectId))

  def membersC(projectId: UUID): ConnectionIO[List[MemberDto]] =
    sql"""select u.id, u.name, u.avatar_url, m.role from project_member m join app_user u on u.id = m.user_id
          where m.project_id = $projectId and not u.disabled order by u.name"""
      .query[(UUID, String, Option[String], String)].map((id, n, a, r) => MemberDto(UserBrief(id, n, a), r)).to[List]

  def setMember(projectId: UUID, userId: UUID, role: Role): IO[Unit] = tx(
    sql"""insert into project_member (project_id, user_id, role) values ($projectId, $userId, ${role.asString})
          on conflict (project_id, user_id) do update set role = excluded.role""".update.run.void
  )

  def removeMember(projectId: UUID, userId: UUID): IO[Unit] = tx(
    sql"delete from project_member where project_id = $projectId and user_id = $userId".update.run.void
  )

  def groupRoles(projectId: UUID): IO[List[GroupRoleDto]] = tx(
    sql"""select g.id, g.name, m.role from project_group_member m join user_group g on g.id = m.group_id
          where m.project_id = $projectId order by g.name""".query[GroupRoleDto].to[List]
  )

  def setGroupRole(projectId: UUID, groupId: UUID, role: Role): IO[Unit] = tx(
    sql"""insert into project_group_member (project_id, group_id, role) values ($projectId, $groupId, ${role.asString})
          on conflict (project_id, group_id) do update set role = excluded.role""".update.run.void
  )

  def removeGroupRole(projectId: UUID, groupId: UUID): IO[Unit] = tx(
    sql"delete from project_group_member where project_id = $projectId and group_id = $groupId".update.run.void
  )

  /** プロジェクトでの役割ごとの人（直接のメンバーと、グループで権限を持つ人）。role が "all" なら全員。 */
  def usersWithRole(projectId: UUID, role: String): IO[List[UUID]] =
    val roles = role match
      case "admin" => List("admin")
      case "edit"  => List("edit", "admin")
      case _       => List("view", "edit", "admin")
    tx(sql"""select m.user_id from project_member m join app_user u on u.id = m.user_id
             where m.project_id = $projectId and m.role = any(${roles.toArray}) and not u.disabled
           union
           select gm.user_id from project_group_member pg join group_member gm on gm.group_id = pg.group_id join app_user u on u.id = gm.user_id
             where pg.project_id = $projectId and pg.role = any(${roles.toArray}) and not u.disabled""".query[UUID].to[List])

  /** グループの人のうち、そのプロジェクトを閲覧できる人。 */
  def groupMembers(projectId: UUID, groupId: UUID, adminGroups: Set[String]): IO[List[UUID]] =
    (tx(sql"select gm.user_id from group_member gm where gm.group_id = $groupId".query[UUID].to[List]), audience(projectId, adminGroups)).mapN {
      (gs, aud) => val ids = aud.map(_.id).toSet; gs.filter(ids)
    }

  /** メンションの候補にするグループ（そのプロジェクトを閲覧できる人がいるもの）と役割。 */
  def mentionGroups(projectId: UUID, adminGroups: Set[String]): IO[List[MentionGroupDto]] =
    for
      aud  <- audience(projectId, adminGroups).map(_.map(_.id).toSet)
      gs   <- tx(sql"select g.id, g.name, gm.user_id from user_group g join group_member gm on gm.group_id = g.id".query[(UUID, String, UUID)].to[List])
      adm  <- usersWithRole(projectId, "admin")
      edt  <- usersWithRole(projectId, "edit")
    yield
      val roles = List(MentionGroupDto("r:admin", "管理者", adm.size), MentionGroupDto("r:edit", "編集者", edt.size),
        MentionGroupDto("r:all", "全員", aud.size)).filter(_.count > 0)
      roles ++ gs.groupBy(g => (g._1, g._2)).toList.map((k, ms) => MentionGroupDto(s"g:${k._1}", k._2, ms.count(m => aud(m._3))))
        .filter(_.count > 0).sortBy(_.name)

  /** プロジェクトを閲覧できる有効なユーザー（メンションの候補・通知の宛先の確認）。ヘルプデスクの"ユーザ"は含めない。 */
  def audience(projectId: UUID, adminGroups: Set[String]): IO[List[UserBrief]] = tx(
    sql"""select distinct u.id, u.name, u.avatar_url from app_user u
          where not u.disabled and (
            u.is_admin
            or exists(select 1 from project_member m where m.project_id = $projectId and m.user_id = u.id and m.role <> 'requester')
            or exists(select 1 from project_group_member pg join group_member gm on gm.group_id = pg.group_id
                      where pg.project_id = $projectId and gm.user_id = u.id and pg.role <> 'requester')
            or exists(select 1 from group_member gm join user_group g on g.id = gm.group_id
                      where gm.user_id = u.id and g.name = any(${adminGroups.toArray})))
          order by u.name""".query[UserBrief].to[List]
  )

  // ---------------------------------------------------------------- ステータス

  def statuses(projectId: UUID): IO[List[StatusDto]] = tx(statusesC(projectId))

  def statusesC(projectId: UUID): ConnectionIO[List[StatusDto]] =
    sql"select id, name, category, color, position from status where project_id = $projectId order by position, name".query[StatusDto].to[List]

  def addStatus(projectId: UUID, r: StatusReq): IO[Either[StoreError, StatusDto]] = tx {
    sql"select exists(select 1 from status where project_id = $projectId and name = ${r.name})".query[Boolean].unique.flatMap { dup =>
      if dup then fail(StoreError.Conflict(s"ステータス「${r.name}」はすでにあります"))
      else
        sql"""insert into status (id, project_id, name, category, color, position)
              values (${UUID.randomUUID()}, $projectId, ${r.name}, ${r.category}, ${r.color.getOrElse("#64748b")},
                      coalesce((select max(position) + 1 from status where project_id = $projectId), 0))
              returning id, name, category, color, position""".query[StatusDto].unique.map(_.asRight[StoreError])
    }
  }

  def updateStatus(projectId: UUID, id: UUID, r: StatusReq): IO[Either[StoreError, Unit]] = tx(updateUniqueC(
    sql"update status set name = ${r.name}, category = ${r.category}, color = ${r.color.getOrElse("#64748b")} where id = $id and project_id = $projectId",
    "ステータス", r.name))

  def reorderStatuses(projectId: UUID, ids: List[UUID]): IO[Unit] = tx(reorderC("status", projectId, ids))

  /** ステータスを削除する。使っているタスクは moveTo に移す。 */
  def deleteStatus(projectId: UUID, id: UUID, moveTo: UUID): IO[Either[StoreError, Unit]] = tx {
    sql"select count(*) from status where project_id = $projectId and id in ($id, $moveTo)".query[Int].unique.flatMap { n =>
      if id == moveTo || n != 2 then fail(StoreError.Invalid("移動先のステータスを指定してください"))
      else
        for
          _ <- sql"update task set status_id = $moveTo where status_id = $id".update.run
          _ <- sql"delete from status where id = $id".update.run
        yield Right(())
    }
  }

  // ---------------------------------------------------------------- ラベル

  // ---------------------------------------------------------------- マイルストーン（ガントの目印）

  def createMilestone(projectId: UUID, name: String, date: LocalDate, color: String): IO[MilestoneDto] = tx {
    val id = UUID.randomUUID()
    sql"insert into milestone (id, project_id, name, date, color) values ($id, $projectId, $name, $date, $color)".update.run
      .as(MilestoneDto(id, name, date.toString, color))
  }

  def updateMilestone(projectId: UUID, id: UUID, name: String, date: LocalDate, color: String): IO[Boolean] = tx(
    sql"update milestone set name = $name, date = $date, color = $color where id = $id and project_id = $projectId".update.run.map(_ > 0)
  )

  def deleteMilestone(projectId: UUID, id: UUID): IO[Boolean] = tx(
    sql"delete from milestone where id = $id and project_id = $projectId".update.run.map(_ > 0)
  )

  def labels(projectId: UUID): IO[List[LabelDto]] = tx(labelsC(projectId))

  def labelsC(projectId: UUID): ConnectionIO[List[LabelDto]] =
    sql"select id, name, color from label where project_id = $projectId order by name".query[LabelDto].to[List]

  /** 名前のラベルを返す（なければ作る）。 */
  def ensureLabelC(projectId: UUID, name: String, color: Option[String]): ConnectionIO[LabelDto] =
    sql"""insert into label (id, project_id, name, color) values (${UUID.randomUUID()}, $projectId, ${name.trim}, ${color.getOrElse("#64748b")})
          on conflict (project_id, name) do update set name = excluded.name returning id, name, color""".query[LabelDto].unique

  def ensureLabel(projectId: UUID, name: String, color: Option[String]): IO[LabelDto] = tx(ensureLabelC(projectId, name, color))

  def updateLabel(projectId: UUID, id: UUID, r: LabelReq): IO[Either[StoreError, Unit]] = tx(updateUniqueC(
    sql"update label set name = ${r.name.trim}, color = ${r.color.getOrElse("#64748b")} where id = $id and project_id = $projectId",
    "ラベル", r.name))

  def deleteLabel(projectId: UUID, id: UUID): IO[Unit] = tx(sql"delete from label where id = $id and project_id = $projectId".update.run.void)

  // ---------------------------------------------------------------- カスタムフィールド

  def fields(projectId: UUID): IO[List[CustomFieldDto]] = tx(fieldsC(projectId))

  def fieldsC(projectId: UUID): ConnectionIO[List[CustomFieldDto]] =
    sql"""select id, name, kind, options, required, position, help_form, help_hint from custom_field
          where project_id = $projectId order by position, name"""
      .query[(UUID, String, String, Json, Boolean, Int, Boolean, String)]
      .map((id, n, k, o, r, p, hf, hh) => CustomFieldDto(id, n, k, o.as[List[String]].getOrElse(Nil), r, p, hf, hh)).to[List]

  def addField(projectId: UUID, r: CustomFieldReq): IO[Either[StoreError, CustomFieldDto]] = tx(
    sql"""insert into custom_field (id, project_id, name, kind, options, required, help_form, help_hint, position)
          values (${UUID.randomUUID()}, $projectId, ${r.name.trim}, ${r.kind}, ${r.options.asJson}, ${r.required},
                  ${r.helpForm.getOrElse(false)}, ${r.helpHint.getOrElse("").trim},
                  coalesce((select max(position) + 1 from custom_field where project_id = $projectId), 0))
          returning id""".query[UUID].unique.attemptSql.flatMap {
      case Left(_)   => fail(StoreError.Conflict(s"フィールド「${r.name}」はすでにあります"))
      case Right(id) => fieldsC(projectId).map(_.find(_.id == id).toRight(StoreError.NotFound("フィールド")))
    }
  )

  def updateField(projectId: UUID, id: UUID, r: CustomFieldReq): IO[Either[StoreError, Unit]] = tx(updateUniqueC(
    sql"""update custom_field set name = ${r.name.trim}, kind = ${r.kind}, options = ${r.options.asJson}, required = ${r.required},
          help_form = coalesce(${r.helpForm}, help_form), help_hint = coalesce(${r.helpHint.map(_.trim)}, help_hint)
          where id = $id and project_id = $projectId""",
    "フィールド", r.name))

  def reorderFields(projectId: UUID, ids: List[UUID]): IO[Unit] = tx(reorderC("custom_field", projectId, ids))

  def deleteField(projectId: UUID, id: UUID): IO[Unit] = tx(sql"delete from custom_field where id = $id and project_id = $projectId".update.run.void)

  // ---------------------------------------------------------------- スプリント

  private val sprintCols = fr"id, project_id, name, goal, state, start_date::text, end_date::text, position, completed_at from sprint"

  def sprints(projectId: UUID, includeClosed: Boolean): IO[List[SprintDto]] = tx(sprintsC(projectId, includeClosed))

  def sprintsC(projectId: UUID, includeClosed: Boolean): ConnectionIO[List[SprintDto]] =
    (fr"select" ++ sprintCols ++ fr"where project_id = $projectId" ++ (if includeClosed then Fragment.empty else fr"and state <> 'closed'") ++
      fr"order by case state when 'active' then 0 when 'planned' then 1 else 2 end, position, coalesce(completed_at, created_at) desc")
      .query[SprintDto].to[List]

  def sprint(id: UUID): IO[Option[SprintDto]] = tx(sprintC(id))

  def sprintC(id: UUID): ConnectionIO[Option[SprintDto]] = (fr"select" ++ sprintCols ++ fr"where id = $id").query[SprintDto].option

  def activeSprintC(projectId: UUID): ConnectionIO[Option[SprintDto]] =
    (fr"select" ++ sprintCols ++ fr"where project_id = $projectId and state = 'active'").query[SprintDto].option

  def createSprint(projectId: UUID, r: SprintReq, start: Option[LocalDate], end: Option[LocalDate]): IO[SprintDto] = tx(
    sql"""insert into sprint (id, project_id, name, goal, start_date, end_date, position)
          values (${UUID.randomUUID()}, $projectId, ${r.name.trim}, ${r.goal.getOrElse("")}, $start, $end,
                  coalesce((select max(position) + 1 from sprint where project_id = $projectId), 0))
          returning id""".query[UUID].unique.flatMap(id => sprintC(id).map(_.get))
  )

  def updateSprint(id: UUID, r: SprintReq, start: Option[LocalDate], end: Option[LocalDate]): IO[Unit] = tx(
    sql"update sprint set name = ${r.name.trim}, goal = ${r.goal.getOrElse("")}, start_date = $start, end_date = $end where id = $id".update.run.void
  )

  def startSprint(projectId: UUID, id: UUID, today: LocalDate): IO[Either[StoreError, SprintDto]] = tx(
    activeSprintC(projectId).flatMap {
      case Some(a) => fail(StoreError.Conflict(s"スプリント「${a.name}」が進行中です。完了してから開始してください"))
      case None =>
        sql"""update sprint set state = 'active', start_date = coalesce(start_date, $today), end_date = coalesce(end_date, $today + 14)
              where id = $id and project_id = $projectId and state = 'planned'""".update.run.flatMap { n =>
          if n == 0 then fail(StoreError.Invalid("開始できるのは計画中のスプリントだけです"))
          else sprintC(id).map(_.toRight(StoreError.NotFound("スプリント")))
        }
    }
  )

  /** 完了したスプリントの未完了のタスクを moveTo（None ならバックログ）へ移す。移したタスクの数を返す。 */
  def completeSprint(projectId: UUID, id: UUID, moveTo: Option[UUID]): IO[Either[StoreError, Int]] = tx(
    sql"update sprint set state = 'closed', completed_at = now() where id = $id and project_id = $projectId and state = 'active'".update.run.flatMap { n =>
      if n == 0 then fail(StoreError.Invalid("完了できるのは進行中のスプリントだけです"))
      else moveTo.filter(_ != id).traverse(m =>
        sql"select exists(select 1 from sprint where id = $m and project_id = $projectId and state <> 'closed')".query[Boolean].unique).flatMap {
        case Some(false) => fail[Int](StoreError.Invalid("移動先のスプリントが見つかりません"))
        case _ =>
        sql"""update task t set sprint_id = $moveTo, updated_at = now() from status s
              where s.id = t.status_id and t.sprint_id = $id and s.category <> 'done' and t.deleted_at is null""".update.run.map(_.asRight)
      }
    }
  )

  def deleteSprint(projectId: UUID, id: UUID): IO[Either[StoreError, Unit]] = tx(
    sql"delete from sprint where id = $id and project_id = $projectId and state <> 'active'".update.run.map(n =>
      if n == 0 then Left(StoreError.Invalid("進行中のスプリントは削除できません")) else Right(()))
  )

  def reorderSprints(projectId: UUID, ids: List[UUID]): IO[Unit] = tx(reorderC("sprint", projectId, ids))

  // ---------------------------------------------------------------- まとめて取得

  def meta(projectId: UUID, role: Role): IO[Option[ProjectMetaDto]] = tx(
    dtoC(projectId, role).flatMap(_.traverse { p =>
      for
        st <- statusesC(projectId)
        lb <- labelsC(projectId)
        sp <- sprintsC(projectId, includeClosed = false)
        fs <- fieldsC(projectId)
        ms <- membersC(projectId).map(_.filterNot(_.role == Roles.Requester))  // 担当者などの候補は、エージェントだけ
        ev <- sql"select coalesce(max(id), 0) from task_event where project_id = $projectId".query[Long].unique
      yield ProjectMetaDto(p, st, lb, sp, fs, ms, ev)
    })
  )
