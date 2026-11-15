package skea.db

import cats.effect.*
import cats.syntax.all.*
import munit.CatsEffectSuite
import skea.core.*
import skea.shared.*

import java.time.LocalDate
import java.util.UUID

/** DB を使うテスト。TEST_DATABASE_URL がなければ飛ばす（例: jdbc:postgresql://localhost:54339/skea_test）。 */
class StoreSpec extends CatsEffectSuite:

  private val cfg: Option[DbConfig] = sys.env.get("TEST_DATABASE_URL").map(url =>
    DbConfig(url, sys.env.getOrElse("TEST_DATABASE_USER", "skea"), sys.env.getOrElse("TEST_DATABASE_PASSWORD", "skea")))

  private val xaFixture = ResourceSuiteLocalFixture("xa",
    cfg.fold(Resource.eval(IO.raiseError[doobie.Transactor[IO]](new RuntimeException("no db"))))(c =>
      Resource.eval(Database.migrate(c)) *> Database.transactor(c)))

  override def munitFixtures = if cfg.isDefined then List(xaFixture) else Nil

  private def withDb(name: String)(body: (UserStore, ProjectStore, TaskStore, SearchStore) => IO[Unit]): Unit =
    test(name) {
      if cfg.isEmpty then IO(assume(false, "TEST_DATABASE_URL が未設定"))
      else
        val xa = xaFixture()
        val tasks = new TaskStore(xa)
        body(new UserStore(xa), new ProjectStore(xa), tasks, new SearchStore(xa, tasks))
    }

  /** 毎回違うキーのプロジェクトを作る。 */
  private def project(users: UserStore, projects: ProjectStore): IO[(User, UUID)] =
    for
      u   <- users.upsert(s"test:${UUID.randomUUID()}", "テスト 太郎", None, None)
      key  = "T" + UUID.randomUUID().toString.filter(_.isLetterOrDigit).toUpperCase.take(8)
      pid <- projects.create(key, "テスト", "", u.id).map(_.toOption.get)
    yield (u, pid)

  withDb("タスクの作成（採番・既定のステータス）と更新・楽観ロック") { (users, projects, tasks, _) =>
    for
      up      <- project(users, projects)
      (u, pid) = up
      t1      <- tasks.create(pid, CreateTaskReq("一つ目"), "", Some(u.id), None, None).map(_.toOption.get)
      t2      <- tasks.create(pid, CreateTaskReq("二つ目"), "", Some(u.id), None, None).map(_.toOption.get)
      sts     <- projects.statuses(pid)
      _        = assertEquals(t2.number, t1.number + 1)
      _        = assertEquals(sts.find(_.id == t1.statusId).map(_.category), Some(StatusCategory.Todo))
      done     = sts.find(_.category == StatusCategory.Done).get
      r       <- tasks.update(t1.id, TaskChanges(statusId = Some(done.id), dueDate = Some(Some(LocalDate.of(2026, 12, 1))), expectedVersion = Some(1)))
      _        = assertEquals(r.map(_.changes.map(_.field).toSet), Right(Set("statusId", "dueDate")))
      _        = assert(r.toOption.get.after.resolvedAt.isDefined)
      stale   <- tasks.update(t1.id, TaskChanges(title = Some("古い版から"), expectedVersion = Some(1)))
      _        = assert(stale.left.exists(_.isInstanceOf[StoreError.Conflict]), stale)
      bad     <- tasks.update(t1.id, TaskChanges(startDate = Some(Some(LocalDate.of(2026, 12, 5)))))
      _        = assert(bad.isLeft, "開始日が期日より後なら拒否する")
    yield ()
  }

  withDb("親子の循環と依存関係の循環を拒否する") { (users, projects, tasks, _) =>
    for
      up      <- project(users, projects)
      (u, pid) = up
      a       <- tasks.create(pid, CreateTaskReq("A"), "", Some(u.id), None, None).map(_.toOption.get)
      b       <- tasks.create(pid, CreateTaskReq("B", parentId = Some(a.id)), "", Some(u.id), None, None).map(_.toOption.get)
      cyc     <- tasks.update(a.id, TaskChanges(parentId = Some(Some(b.id))))
      _        = assert(cyc.isLeft, "子孫を親にはできない")
      d1      <- tasks.addDependency(a.id, b.id, "fs")
      d2      <- tasks.addDependency(b.id, a.id, "fs")
      _        = assert(d1.isRight)
      _        = assert(d2.isLeft, "依存関係の循環は拒否する")
    yield ()
  }

  withDb("並び順の移動（前後の間に入る）") { (users, projects, tasks, _) =>
    for
      up      <- project(users, projects)
      (u, pid) = up
      ts      <- List("1", "2", "3").traverseIO(n => tasks.create(pid, CreateTaskReq(n), "", Some(u.id), None, None).map(_.toOption.get))
      _       <- tasks.move(ts(2).id, MoveTaskReq(None, None, None, Some(ts(0).id), Some(ts(1).id)))
      order   <- tasks.backlog(pid).map(_.map(_.title))
      _        = assertEquals(order, List("1", "3", "2"))
    yield ()
  }

  withDb("日本語の検索（かなの揺れ・入力ミス・絞り込み）") { (users, projects, tasks, search) =>
    for
      up      <- project(users, projects)
      (u, pid) = up
      _       <- tasks.create(pid, CreateTaskReq("ログイン画面のバグ"), "パスワードでエラー", Some(u.id), None, None)
      _       <- tasks.create(pid, CreateTaskReq("デザインの確認"), "", Some(u.id), None, None)
      ctx      = search.Ctx(u.id, List(pid), LocalDate.of(2026, 11, 15))
      q        = (s: String) => search.search(SearchQuery.parse(s).toOption.get, ctx, 50, 0).map(_.toOption.get)
      r1      <- q("ろぐいん")
      _        = assertEquals(r1.hits.map(_.task.title), List("ログイン画面のバグ"))
      r2      <- q("デザイソ")
      _        = assertEquals(r2.hits.map(h => (h.task.title, h.fuzzy)), List(("デザインの確認", true)))
      r3      <- q("assignee:none is:open order:-key")
      _        = assertEquals(r3.hits.map(_.task.title), List("デザインの確認", "ログイン画面のバグ"))
      r4      <- q("ぱすわーど")
      _        = assertEquals(r4.hits.size, 1)
    yield ()
  }

  extension [A](as: List[A]) private def traverseIO[B](f: A => IO[B]): IO[List[B]] =
    as.foldLeft(IO.pure(List.empty[B]))((acc, a) => acc.flatMap(l => f(a).map(l :+ _)))

  withDb("コメントの下書き（ユーザー＋タスクごと）・マイルストーン・アイコン") { (users, projects, tasks, _) =>
    import io.circe.Json
    for
      up      <- project(users, projects)
      (u, pid) = up
      other   <- users.upsert(s"test:${UUID.randomUUID()}", "別の 人", None, None)
      t       <- tasks.create(pid, CreateTaskReq("下書きの確認"), "", Some(u.id), None, None).map(_.toOption.get)
      doc      = RichText.fromPlain("書きかけ")
      _       <- tasks.saveDraft(u.id, t.id, doc)
      _       <- tasks.saveDraft(u.id, t.id, RichText.fromPlain("書きかけ（続き）"))
      mine    <- tasks.draft(u.id, t.id)
      theirs  <- tasks.draft(other.id, t.id)
      _        = assertEquals(mine.map(d => RichText.toPlain(d.body)), Some("書きかけ（続き）"))
      _        = assertEquals(theirs, None)
      _       <- tasks.deleteDraft(u.id, t.id)
      gone    <- tasks.draft(u.id, t.id)
      _        = assertEquals(gone, None)
      m       <- projects.createMilestone(pid, "リリース", LocalDate.of(2026, 12, 24), "#2563eb")
      g       <- tasks.gantt(pid, includeDone = false)
      _        = assertEquals(g.milestones.map(x => (x.name, x.date)), List(("リリース", "2026-12-24")))
      ok      <- projects.updateMilestone(pid, m.id, "v1.0", LocalDate.of(2026, 12, 25), "#2563eb")
      del     <- projects.deleteMilestone(pid, m.id)
      _        = assert(ok && del)
      old     <- users.setAvatar(u.id, Some("avatars/a"), Some("/api/users/x/avatar?v=1"))
      _        = assertEquals(old, None)
      again   <- users.upsert(u.sub, u.name, None, Some("https://idp.example/pic.png"))
      _        = assertEquals(again.avatarUrl, Some("/api/users/x/avatar?v=1"), "登録したアイコンは IdP の画像で上書きしない")
      prev    <- users.setAvatar(u.id, None, None)
      _        = assertEquals(prev, Some("avatars/a"))
    yield ()
  }

  withDb("コメントのリアクションと、テンプレート") { (users, projects, tasks, _) =>
    for
      up      <- project(users, projects)
      (u, pid) = up
      t       <- tasks.create(pid, CreateTaskReq("返信の確認"), "", Some(u.id), None, None).map(_.toOption.get)
      c1      <- tasks.addComment(t.id, Some(u.id), RichText.fromPlain("元"), "元", "web")
      _       <- tasks.react(c1.id, u.id, "👍", on = true)
      _       <- tasks.react(c1.id, u.id, "👍", on = true)
      _       <- tasks.react(c1.id, u.id, "🎉", on = true)
      cs      <- tasks.comments(t.id)
      _        = assertEquals(cs.find(_.id == c1.id).map(_.reactions.map(r => (r.emoji, r.users.size))), Some(List(("👍", 1), ("🎉", 1))))
      _       <- tasks.react(c1.id, u.id, "👍", on = false)
      after   <- tasks.comment(c1.id)
      _        = assertEquals(after.map(_.reactions.map(_.emoji)), Some(List("🎉")))
      // 優先度と種類（作成時の指定・更新・検索）
      pt      <- tasks.create(pid, CreateTaskReq("優先度の確認", priority = Some("high"), taskType = Some("bug")), "", Some(u.id), None, None).map(_.toOption.get)
      _        = assertEquals((pt.priority, pt.taskType), ("high", "bug"))
      upd     <- tasks.update(pt.id, TaskChanges(priority = Some("lowest")))
      _        = assertEquals(upd.map(_.changes.map(_.field)), Right(List("priority")))
      store    = new TemplateStore(xaFixture())
      id      <- store.createTemplate(pid, TemplateReq("定例", "定例 {{date}}", subtasks = List("議事録", " ")), u.id).map(_.toOption.get)
      dup     <- store.createTemplate(pid, TemplateReq("定例", "別"), u.id)
      _        = assert(dup.isLeft, "同じ名前は作れない")
      tpl     <- store.template(id)
      _        = assertEquals(tpl.map(_._1.subtasks), Some(List("議事録")))
      _       <- store.createRecurring(pid, RecurringReq(id, "weekly", Some(1), None, false, true, None), LocalDate.of(2026, 11, 16), u.id)
      due     <- store.dueRecurring(LocalDate.of(2026, 11, 16))
      mine     = due.filter(_.projectId == pid)
      _        = assertEquals(mine.size, 1)
      ok      <- store.markRun(mine.head.id, LocalDate.of(2026, 11, 16), LocalDate.of(2026, 11, 23), Some("X-1"))
      again   <- store.markRun(mine.head.id, LocalDate.of(2026, 11, 16), LocalDate.of(2026, 11, 23), Some("X-1"))
      _        = assert(ok && !again, "同じ回は1度だけ進める")
    yield ()
  }

  withDb("OAuth: 認可コードは一度だけ・期限内だけ使え、許可し直すと前のキーを失効させる") { (users, projects, _, _) =>
    for
      up      <- project(users, projects)
      (u, _)   = up
      store    = new OAuthStore(xaFixture())
      code     = OAuthCode("skiwi", "https://w.example/cb", u.id, "challenge", "read")
      h1       = java.util.UUID.randomUUID().toString.getBytes
      h2       = java.util.UUID.randomUUID().toString.getBytes
      _       <- store.createCode(h1, code, java.time.Instant.now().plusSeconds(60))
      _       <- store.createCode(h2, code, java.time.Instant.now().minusSeconds(1))
      first   <- store.consumeCode(h1)
      again   <- store.consumeCode(h1)
      expired <- store.consumeCode(h2)
      _        = assertEquals((first.map(_.userId), again, expired), (Some(u.id), None, None))
      r1       = java.util.UUID.randomUUID().toString.getBytes
      r2       = java.util.UUID.randomUUID().toString.getBytes
      k1      <- store.replaceKey(u.id, "skiwi", "skiwi 連携", "skea_aaaaaa", r1, List("read"))
      k2      <- store.replaceKey(u.id, "skiwi", "skiwi 連携", "skea_bbbbbb", r2, List("read"))
      keys    <- users.keys(u.id)
      _        = assertEquals(keys.map(_.id), List(k2), "許可し直すと前のキーは失効する")
      _       <- store.revokeKey(r2, "other-client")
      still   <- users.keys(u.id)
      _        = assertEquals(still.size, 1, "ほかのクライアントからは失効できない")
      _       <- store.revokeKey(r2, "skiwi")
      none    <- users.keys(u.id)
      _        = assert(none.isEmpty && k1 != k2)
    yield ()
  }
