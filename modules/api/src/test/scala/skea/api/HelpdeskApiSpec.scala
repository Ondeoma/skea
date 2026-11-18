package skea.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.implicits.*
import skea.core.*
import skea.db.*
import skea.shared.*

import java.util.UUID

/**
 * ヘルプデスクの API: "ユーザ"（requester）に、内部コメント・共有されていないタスク・エージェント向けの API が見えないこと。
 * DB を使う（ApiDbSuite）。
 */
object HelpdeskApiSpec:
  /** テスト用のメモリ上の保存先（S3 の代わり）。 */
  final class MemoryBlobs extends skea.storage.BlobStore:
    private val m = new java.util.concurrent.ConcurrentHashMap[String, (String, Array[Byte])]()
    def put(key: String, file: fs2.io.file.Path, contentType: String): IO[Unit] =
      IO.blocking(java.nio.file.Files.readAllBytes(file.toNioPath)).map(b => m.put(key, (contentType, b))).void
    def get(key: String): IO[Option[(skea.storage.BlobMeta, fs2.Stream[IO, Byte])]] =
      IO(Option(m.get(key)).map((ct, b) => (skea.storage.BlobMeta(ct, b.length.toLong), fs2.Stream.emits(b.toSeq))))
    def delete(key: String): IO[Unit] = IO(m.remove(key)).void
    def copy(from: String, to: String): IO[Boolean] = IO(Option(m.get(from)).exists(v => { m.put(to, v); true }))
    def keys: Set[String] = scala.jdk.CollectionConverters.SetHasAsScala(m.keySet()).asScala.toSet

class HelpdeskApiSpec extends ApiDbSuite:

  private def doc(text: String): Json = RichText.fromPlain(text)

  private def keys(j: Json): Set[String] = j.asArray.toList.flatten.flatMap(_.hcursor.get[String]("key").toOption).toSet

  withEnv("\"ユーザ\"には内部コメント・共有されていないタスク・エージェント向けの API が見えない") { env =>
    for
      a        <- env.user("エージェント")
      (agent, at) = a
      r        <- env.user("ユーザ 一郎")
      (req1, rt) = r
      o        <- env.user("ユーザ 二郎")
      (req2, ot) = o
      key       = "H" + UUID.randomUUID().toString.filter(_.isLetterOrDigit).toUpperCase.take(8)
      pid      <- env.projects.create(key, "ヘルプデスク", "", agent.id).map(_.toOption.get)
      // ヘルプデスクを有効にする前は"ユーザ"を付けられない
      bad      <- env.put(at, s"/api/projects/$pid/members", SetMemberReq(req1.id, Roles.Requester).asJson)
      _         = assertEquals(bad._1, Status.BadRequest)
      _        <- env.put(at, s"/api/projects/$pid/helpdesk", HelpdeskSettingsReq(true, true, Nil).asJson).map(r => assertEquals(r._1, Status.NoContent))
      _        <- env.put(at, s"/api/projects/$pid/members", SetMemberReq(req1.id, Roles.Requester).asJson).map(r => assertEquals(r._1, Status.NoContent))
      // req2 は窓口からゲスト登録する
      joined   <- env.post(ot, s"/api/help/portal/$key/join", Json.obj())
      _         = assertEquals((joined._1, joined._2.hcursor.get[String]("role").toOption), (Status.Ok, Some(Roles.Requester)))
      // 問い合わせ（req1）と、共有していないエージェントのタスク
      created  <- env.post(rt, s"/api/help/projects/$pid/tasks", HelpRequestReq("ログインできません", Some(doc("パスワードを忘れました"))).asJson)
      _         = assertEquals(created._1, Status.Created)
      reqKey    = created._2.hcursor.get[String]("key").toOption.get
      inner    <- env.post(at, s"/api/projects/$pid/tasks", CreateTaskReq("社内の作業").asJson)
      innerKey  = inner._2.hcursor.get[String]("key").toOption.get
      // エージェントの内部コメントと公開の回答
      _        <- env.post(at, s"/api/tasks/$reqKey/comments", CommentReq(doc("社外秘のメモ"), Some(true)).asJson).map(r => assertEquals(r._1, Status.Created))
      _        <- env.post(at, s"/api/tasks/$reqKey/comments", CommentReq(doc("リセットしました")).asJson).map(r => assertEquals(r._1, Status.Created))

      // ---- "ユーザ"の窓口
      list     <- env.get(rt, s"/api/help/projects/$pid/tasks")
      _         = assertEquals(keys(list._2), Set(reqKey))
      det      <- env.get(rt, s"/api/help/tasks/$reqKey")
      _         = assertEquals(det._1, Status.Ok)
      texts     = det._2.hcursor.downField("comments").as[List[CommentDto]].toOption.get.map(_.html)
      _         = assert(texts.exists(_.contains("リセットしました")) && !texts.exists(_.contains("社外秘")), texts)
      _         = assert(!det._2.noSpaces.contains("社外秘"))
      _        <- env.get(rt, s"/api/help/tasks/$innerKey").map(r => assertEquals(r._1, Status.NotFound, "共有されていないタスク"))
      _        <- env.get(ot, s"/api/help/tasks/$reqKey").map(r => assertEquals(r._1, Status.NotFound, "ほかの\"ユーザ\"の問い合わせ"))
      // 返信の内部フラグは無視される（"ユーザ"は内部コメントを書けない）
      rep      <- env.post(rt, s"/api/help/tasks/$reqKey/comments", CommentReq(doc("ありがとう"), Some(true)).asJson)
      _         = assertEquals(rep._2.hcursor.get[Boolean]("internal").toOption, Some(false))

      // ---- 窓口の検索: 自分に見えるものの公開の部分だけで探す（内部コメント・共有されていないタスクの語では当たらない）
      search    = (t: String, q: String) => env.get(t, s"/api/help/projects/$pid/search?q=${java.net.URLEncoder.encode(q, "UTF-8")}")
      hits      = (j: Json) => j.hcursor.downField("hits").as[List[HelpHitDto]].toOption.getOrElse(Nil).map(_.task.key).toSet
      _        <- search(rt, "リセット").map(r => assertEquals(hits(r._2), Set(reqKey), "公開の回答の語で当たる"))
      _        <- search(rt, "パスワード").map(r => assertEquals(hits(r._2), Set(reqKey), "説明の語で当たる"))
      _        <- search(rt, "社外秘").map(r => assertEquals(hits(r._2), Set.empty[String], "内部コメントの語では当たらない"))
      _        <- search(rt, "社外比").map(r => assertEquals(hits(r._2), Set.empty[String], "タイプミスを許す一致でも内部コメントには当たらない"))
      _        <- search(rt, "comment:社外秘").map(r => assertEquals(hits(r._2), Set.empty[String]))
      _        <- search(rt, "社内").map(r => assertEquals(hits(r._2), Set.empty[String], "共有されていないタスクは当たらない"))
      _        <- search(rt, s"key:$innerKey").map(r => assertEquals(hits(r._2), Set.empty[String]))
      _        <- search(rt, "is:mine has:comments order:-created").map(r => assertEquals(hits(r._2), Set(reqKey)))
      _        <- search(rt, "-リセット").map(r => assertEquals(hits(r._2), Set.empty[String], "除外の語は公開の回答も見る"))
      lab      <- search(rt, "label:バグ")
      _         = assert(lab._2.hcursor.get[String]("error").toOption.exists(_.contains("label")), lab)
      ord      <- search(rt, "order:priority")
      _         = assert(ord._2.hcursor.get[String]("error").isRight, ord)
      _        <- search(ot, "ログイン").map(r => assertEquals(hits(r._2), Set.empty[String], "ほかの\"ユーザ\"の問い合わせは当たらない"))

      // ---- エージェント向けの API は使えない（存在も知らせない）
      agentApis = List(s"/api/tasks/$reqKey", s"/api/tasks/$reqKey/comments", s"/api/tasks/$reqKey/history", s"/api/tasks/$reqKey/shares",
                    s"/api/tasks/$innerKey", s"/api/projects/$pid", s"/api/projects/$pid/meta", s"/api/projects/$pid/board",
                    s"/api/projects/$pid/backlog", s"/api/projects/$pid/changes?since=0", s"/api/projects/$pid/activity",
                    s"/api/projects/$pid/members", s"/api/projects/$pid/helpdesk", s"/api/projects/by-key/$key")
      codes    <- agentApis.traverse(p => env.get(rt, p).map(p -> _._1))
      _         = assertEquals(codes.filter(_._2 != Status.NotFound), Nil)
      plist    <- env.get(rt, "/api/projects")
      _         = assert(!plist._2.noSpaces.contains(pid.toString), "プロジェクトの一覧に出さない")
      found    <- env.get(rt, s"/api/search?q=${java.net.URLEncoder.encode("社内", "UTF-8")}")
      _         = assertEquals(found._2.hcursor.downField("items").focus.flatMap(_.asArray).map(_.size).getOrElse(0), 0)
      _        <- env.post(rt, s"/api/projects/$pid/tasks", CreateTaskReq("勝手に作る").asJson).map(r => assertEquals(r._1, Status.NotFound))
      _        <- env.post(rt, s"/api/tasks/$reqKey/comments", CommentReq(doc("直接")).asJson).map(r => assertEquals(r._1, Status.NotFound))

      // ---- エージェントは内部コメントも見え、共有すると"ユーザ"に見える
      all      <- env.get(at, s"/api/tasks/$reqKey/comments")
      _         = assert(all._2.noSpaces.contains("社外秘"))
      shared   <- env.put(at, s"/api/tasks/$innerKey/shares", TaskSharesReq(Nil, List(req2.id), notifyNew = true).asJson)
      _         = assertEquals(shared._1, Status.Ok)
      _        <- env.get(ot, s"/api/help/tasks/$innerKey").map(r => assertEquals(r._1, Status.Ok))
      _        <- env.get(ot, s"/api/help/projects/$pid/tasks").map(r => assertEquals(keys(r._2), Set(innerKey)))
      _        <- search(ot, "is:shared 社内").map(r => assertEquals(hits(r._2), Set(innerKey), "共有されると検索でも見つかる"))

      // ---- 通知: req1 には公開の回答だけ（内部コメントは届かない）、リンクは窓口の画面
      n1       <- env.activity.list(req1.id, Nil, unreadOnly = false, List(pid), None, 50)
      _         = assert(n1.items.nonEmpty && n1.items.forall(n => !n.body.contains("社外秘")), n1.items)
      _         = assert(n1.items.exists(_.kind == NotificationKind.Comment))
      n2       <- env.activity.list(req2.id, Nil, unreadOnly = false, List(pid), None, 50)
      _         = assertEquals(n2.items.map(_.kind), List(NotificationKind.Shared))
      na       <- env.activity.list(agent.id, Nil, unreadOnly = false, List(pid), None, 50)
      _         = assert(na.items.exists(_.kind == NotificationKind.Request), "新しい問い合わせはエージェントに届く")
    yield ()
  }

  withEnv("ゲスト登録: 許可していない窓口・ドメインの違う人は参加できない") { env =>
    for
      a        <- env.user("エージェント")
      (agent, at) = a
      g        <- env.user("ゲスト")
      (_, gt)   = g
      key       = "G" + UUID.randomUUID().toString.filter(_.isLetterOrDigit).toUpperCase.take(8)
      pid      <- env.projects.create(key, "窓口", "", agent.id).map(_.toOption.get)
      _        <- env.post(gt, s"/api/help/portal/$key/join", Json.obj()).map(r => assertEquals(r._1, Status.NotFound, "ヘルプデスクでない"))
      _        <- env.put(at, s"/api/projects/$pid/helpdesk", HelpdeskSettingsReq(true, false, Nil).asJson)
      _        <- env.get(gt, s"/api/help/portal/$key").map(r => assertEquals(r._1, Status.NotFound, "ゲスト登録を許可していない窓口は名前も出さない"))
      _        <- env.put(at, s"/api/projects/$pid/helpdesk", HelpdeskSettingsReq(true, true, List("corp.example.jp")).asJson)
      _        <- env.post(gt, s"/api/help/portal/$key/join", Json.obj()).map(r => assertEquals(r._1, Status.Forbidden, "ドメインが違う"))
      _        <- env.put(at, s"/api/projects/$pid/helpdesk", HelpdeskSettingsReq(true, true, List("example.com")).asJson)
      _        <- env.post(gt, s"/api/help/portal/$key/join", Json.obj()).map(r => assertEquals(r._1, Status.Ok))
      _        <- env.get(gt, s"/api/help/projects").map(r => assert(r._2.noSpaces.contains(key)))
      // 管理者の設定は"ユーザ"には使えない
      _        <- env.put(gt, s"/api/projects/$pid/helpdesk", HelpdeskSettingsReq(true, true, Nil).asJson).map(r => assertEquals(r._1, Status.NotFound))
    yield ()
  }

  withEnv("ヘルプデスク: すべてのタスクが1つのアクティブスプリントに入り、ボードには完了して日数を過ぎたタスクを出さない") { env =>
    def sprintOf(key: String) = env.tasks.coreByKey(key).map(_.flatMap(_.sprintId))
    def boardKeys(t: String, pid: UUID) = env.get(t, s"/api/projects/$pid/board").map(r =>
      r._2.hcursor.downField("tasks").as[List[TaskDto]].toOption.getOrElse(Nil).map(_.key).toSet)
    for
      a        <- env.user("エージェント")
      (agent, at) = a
      key       = "S" + UUID.randomUUID().toString.filter(_.isLetterOrDigit).toUpperCase.take(8)
      pid      <- env.projects.create(key, "スプリントの確認", "", agent.id).map(_.toOption.get)
      // ヘルプデスクにする前: バックログのタスクと、計画中のスプリントのタスク
      planned  <- env.post(at, s"/api/projects/$pid/sprints", SprintReq("計画中", None, None, None).asJson).map(_._2.hcursor.get[UUID]("id").toOption.get)
      b1       <- env.post(at, s"/api/projects/$pid/tasks", CreateTaskReq("バックログのタスク").asJson).map(_._2.hcursor.get[String]("key").toOption.get)
      p1       <- env.post(at, s"/api/projects/$pid/tasks", CreateTaskReq("計画中のタスク", sprintId = Some(planned)).asJson).map(_._2.hcursor.get[String]("key").toOption.get)
      _        <- sprintOf(b1).map(sp => assertEquals(sp, None))
      _        <- env.put(at, s"/api/projects/$pid/helpdesk", HelpdeskSettingsReq(true, false, Nil, Some(7)).asJson).map(r => assertEquals(r._1, Status.NoContent))
      ss       <- env.projects.sprints(pid, includeClosed = true)
      active    = ss.filter(_.state == "active")
      _         = assertEquals(active.size, 1, "アクティブスプリントは1つ")
      sid       = active.head.id
      _        <- (sprintOf(b1), sprintOf(p1)).tupled.map(x => assertEquals(x, (Some(sid), Some(sid)), "既存のタスクはすべてアクティブスプリントへ"))
      // 作成・バックログへの移動・別のスプリントの指定をしても、アクティブスプリントに入る
      n1       <- env.post(at, s"/api/projects/$pid/tasks", CreateTaskReq("新しいタスク").asJson).map(_._2.hcursor.get[String]("key").toOption.get)
      _        <- sprintOf(n1).map(sp => assertEquals(sp, Some(sid)))
      t1       <- env.tasks.coreByKey(n1).map(_.get)
      _        <- env.post(at, s"/api/tasks/${t1.id}/move", MoveTaskReq(None, None, Some(true), None, None).asJson).map(r => assertEquals(r._1, Status.Ok))
      _        <- sprintOf(n1).map(sp => assertEquals(sp, Some(sid), "バックログには移らない"))
      _        <- env.call(at, Method.PATCH, s"/api/tasks/$n1", Some(Json.obj("sprintId" -> Json.Null)))
      _        <- sprintOf(n1).map(sp => assertEquals(sp, Some(sid)))
      // スプリントの操作は自動で管理するので受け付けない（DB でも完了させない）
      _        <- env.post(at, s"/api/projects/$pid/sprints/$sid/complete", CompleteSprintReq(None).asJson).map(r => assertEquals(r._1, Status.BadRequest))
      _        <- env.post(at, s"/api/projects/$pid/sprints", SprintReq("追加", None, None, None).asJson).map(r => assertEquals(r._1, Status.BadRequest))
      _        <- env.call(at, Method.DELETE, s"/api/projects/$pid/sprints/$sid").map(r => assertEquals(r._1, Status.BadRequest))
      direct   <- env.projects.completeSprint(pid, sid, None).attempt
      _         = assert(direct.isLeft || direct.exists(_.isLeft), "DB でもアクティブスプリントを完了させない")
      // ボード: 完了したタスクは、設定の日数を過ぎると出さない
      done     <- env.projects.statuses(pid).map(_.find(_.category == StatusCategory.Done).get)
      _        <- env.call(at, Method.PATCH, s"/api/tasks/$b1", Some(Json.obj("statusId" -> done.id.asJson))).map(r => assertEquals(r._1, Status.Ok))
      _        <- boardKeys(at, pid).map(ks => assertEquals(ks, Set(b1, p1, n1), "完了したばかりのタスクは出す"))
      _        <- env.put(at, s"/api/projects/$pid/helpdesk", HelpdeskSettingsReq(true, false, Nil, Some(0)).asJson)
      _        <- boardKeys(at, pid).map(ks => assertEquals(ks, Set(p1, n1), "日数を過ぎた完了のタスクは出さない"))
      bad      <- env.put(at, s"/api/projects/$pid/helpdesk", HelpdeskSettingsReq(true, false, Nil, Some(-1)).asJson)
      _         = assertEquals(bad._1, Status.BadRequest)
      _        <- env.projects.sprints(pid, includeClosed = true).map(x => assertEquals(x.count(_.state == "active"), 1))
    yield ()
  }

  withEnv("窓口の共有: 自分のメールと同じドメインのグループにだけ共有でき、共有したグループの人はウォッチャーになる") { env =>
    val dom = s"${UUID.randomUUID().toString.take(6)}.example.jp"
    def gid(r: (Status, Json)) = r._2.hcursor.get[UUID]("id").toOption.get
    def shares(j: Json) = j.as[HelpSharesDto].toOption.get
    for
      a        <- env.user("エージェント")
      (agent, at) = a
      r1       <- env.user("A社 一郎", Some(s"ichiro@$dom"))
      (u1, t1) = r1
      r2       <- env.user("A社 二郎", Some(s"jiro@$dom"))
      (u2, t2) = r2
      key       = "D" + UUID.randomUUID().toString.filter(_.isLetterOrDigit).toUpperCase.take(8)
      pid      <- env.projects.create(key, "ドメイン", "", agent.id).map(_.toOption.get)
      _        <- env.put(at, s"/api/projects/$pid/helpdesk", HelpdeskSettingsReq(true, false, Nil).asJson)
      _        <- List(u1, u2).traverse_(u => env.put(at, s"/api/projects/$pid/members", SetMemberReq(u.id, Roles.Requester).asJson))
      gA       <- env.post(at, s"/api/projects/$pid/requester-groups", RequesterGroupReq("A社", None, Some("@" + dom.toUpperCase)).asJson).map(gid)
      gAcc     <- env.post(at, s"/api/projects/$pid/requester-groups", RequesterGroupReq("経理部", Some(gA)).asJson).map(gid)
      gB       <- env.post(at, s"/api/projects/$pid/requester-groups", RequesterGroupReq("B社", None, Some("b-sha.example.com")).asJson).map(gid)
      _        <- env.put(at, s"/api/projects/$pid/requester-groups/$gAcc/members", GroupMemberReq(u2.id).asJson)
      req      <- env.post(t1, s"/api/help/projects/$pid/tasks", HelpRequestReq("プリンター", None).asJson)
      k         = req._2.hcursor.get[String]("key").toOption.get
      // 共有できるのは同じドメインのグループ（子は親のドメインを引き継ぐ）だけ
      opt      <- env.get(t1, s"/api/help/tasks/$k/shares").map(r => shares(r._2))
      _         = assert(opt.canShare)
      _         = assertEquals(opt.options.map(_.groupId).toSet, Set(gA, gAcc))
      _        <- env.put(t1, s"/api/help/tasks/$k/shares", HelpSharesReq(List(gB)).asJson).map(r => assertEquals(r._1, Status.Forbidden))
      _        <- env.get(t2, s"/api/help/tasks/$k").map(r => assertEquals(r._1, Status.NotFound, "共有する前は見えない"))
      sh       <- env.put(t1, s"/api/help/tasks/$k/shares", HelpSharesReq(List(gA)).asJson)
      _         = assertEquals(sh._1, Status.Ok)
      _        <- env.get(t2, s"/api/help/tasks/$k").map(r => assertEquals(r._1, Status.Ok, "親のグループに共有すると子のメンバーにも見える"))
      task     <- env.tasks.coreByKey(k).map(_.get)
      ws       <- env.tasks.watchers(task.id)
      _         = assert(ws.contains(u2.id), "共有したグループの人はウォッチャーになる")
      n2       <- env.activity.list(u2.id, Nil, unreadOnly = false, List(pid), None, 50)
      _         = assert(n2.items.exists(_.kind == NotificationKind.Shared))
      // ほかの人の問い合わせは共有できない
      _        <- env.get(t2, s"/api/help/tasks/$k/shares").map(r => assert(!shares(r._2).canShare))
      _        <- env.put(t2, s"/api/help/tasks/$k/shares", HelpSharesReq(List(gAcc)).asJson).map(r => assertEquals(r._1, Status.Forbidden))
      // エージェントが共有したもの（B社）は"ユーザ"には外せない。自分の共有（A社）は外せる
      _        <- env.put(at, s"/api/tasks/$k/shares", TaskSharesReq(List(gA, gB), Nil, notifyNew = false).asJson).map(r => assertEquals(r._1, Status.Ok))
      cur      <- env.get(t1, s"/api/help/tasks/$k/shares").map(r => shares(r._2).current.map(c => c.groupId -> c.removable).toMap)
      _         = assertEquals(cur, Map(gA -> true, gB -> false), "変わらない共有は共有した人も変わらない")
      _        <- env.put(t1, s"/api/help/tasks/$k/shares", HelpSharesReq(Nil).asJson).map(r => assertEquals(r._1, Status.Ok))
      left     <- env.help.groupShares(task.id).map(_.map(_._1).toSet)
      _         = assertEquals(left, Set(gB))
      // エージェントは watch=false でウォッチャーにしないで共有できる
      other    <- env.post(at, s"/api/projects/$pid/tasks", CreateTaskReq("お知らせ").asJson).map(_._2.hcursor.get[String]("key").toOption.get)
      _        <- env.put(at, s"/api/tasks/$other/shares", TaskSharesReq(List(gAcc), Nil, notifyNew = false, watch = Some(false)).asJson)
      ot       <- env.tasks.coreByKey(other).map(_.get)
      ws2      <- env.tasks.watchers(ot.id)
      _         = assert(!ws2.contains(u2.id))
      // 作成と同時に共有する（候補は同じドメインのグループ。違うドメインを含めば作らない）
      os       <- env.get(t1, s"/api/help/projects/$pid/share-options").map(_._2.as[List[HelpShareItemDto]].toOption.get.map(_.groupId).toSet)
      _         = assertEquals(os, Set(gA, gAcc))
      before   <- env.help.visibleTasks(pid, u1.id, done = true).map(_.size)
      ng       <- env.post(t1, s"/api/help/projects/$pid/tasks", HelpRequestReq("B社にも", None, Some(List(gB))).asJson)
      _         = assertEquals(ng._1, Status.Forbidden)
      _        <- env.help.visibleTasks(pid, u1.id, done = true).map(n => assertEquals(n.size, before, "共有できないときは作らない"))
      cr       <- env.post(t1, s"/api/help/projects/$pid/tasks", HelpRequestReq("経理部にも見せる", None, Some(List(gAcc))).asJson)
      _         = assertEquals(cr._1, Status.Created)
      ck        = cr._2.hcursor.get[String]("key").toOption.get
      _        <- env.get(t2, s"/api/help/tasks/$ck").map(r => assertEquals(r._1, Status.Ok, "作成と同時に共有したグループの人に見える"))
      ct       <- env.tasks.coreByKey(ck).map(_.get)
      _        <- env.tasks.watchers(ct.id).map(w => assert(w.contains(u2.id) && w.contains(u1.id)))
      _        <- env.get(t1, s"/api/help/tasks/$ck/shares").map(r => assertEquals(shares(r._2).current.map(c => c.groupId -> c.removable), List(gAcc -> true)))
    yield ()
  }

  withEnv("問い合わせのフォーム: 選んだカスタムフィールドだけを入力でき、必須を確かめ、値は\"ユーザ\"にも見える") { env =>
    for
      a        <- env.user("エージェント")
      (agent, at) = a
      r        <- env.user("ユーザ")
      (u, rt)   = r
      key       = "F" + UUID.randomUUID().toString.filter(_.isLetterOrDigit).toUpperCase.take(8)
      pid      <- env.projects.create(key, "フォーム", "", agent.id).map(_.toOption.get)
      _        <- env.put(at, s"/api/projects/$pid/helpdesk", HelpdeskSettingsReq(true, false, Nil).asJson)
      _        <- env.put(at, s"/api/projects/$pid/members", SetMemberReq(u.id, Roles.Requester).asJson)
      pc       <- env.post(at, s"/api/projects/$pid/fields",
                    CustomFieldReq("PC の番号", FieldKind.Text, Nil, required = true, Some(true), Some("本体のシールの番号")).asJson).map(_._2.as[CustomFieldDto].toOption.get)
      urg      <- env.post(at, s"/api/projects/$pid/fields",
                    CustomFieldReq("緊急度", FieldKind.Select, List("高", "低"), required = false, Some(true)).asJson).map(_._2.as[CustomFieldDto].toOption.get)
      cost     <- env.post(at, s"/api/projects/$pid/fields",
                    CustomFieldReq("社内の見積もり", FieldKind.Text, Nil, required = false).asJson).map(_._2.as[CustomFieldDto].toOption.get)
      form     <- env.get(rt, s"/api/help/projects/$pid/form").map(_._2.as[List[CustomFieldDto]].toOption.get)
      _         = assertEquals(form.map(_.name), List("PC の番号", "緊急度"), "フォームに出す項目だけ（並び順のとおり）")
      _         = assertEquals(form.head.helpHint, "本体のシールの番号")
      noReq    <- env.post(rt, s"/api/help/projects/$pid/tasks", HelpRequestReq("動かない", None, fields = Some(Map(urg.id.toString -> "高".asJson))).asJson)
      _         = assertEquals(noReq._1, Status.BadRequest, "必須の項目が空")
      hidden   <- env.post(rt, s"/api/help/projects/$pid/tasks",
                    HelpRequestReq("動かない", None, fields = Some(Map(pc.id.toString -> "A-1".asJson, cost.id.toString -> "10万".asJson))).asJson)
      _         = assertEquals(hidden._1, Status.BadRequest, "フォームにない項目は入力できない")
      badOpt   <- env.post(rt, s"/api/help/projects/$pid/tasks",
                    HelpRequestReq("動かない", None, fields = Some(Map(pc.id.toString -> "A-1".asJson, urg.id.toString -> "最高".asJson))).asJson)
      _         = assertEquals(badOpt._1, Status.BadRequest, "選択肢にない値")
      ok       <- env.post(rt, s"/api/help/projects/$pid/tasks",
                    HelpRequestReq("動かない", None, fields = Some(Map(pc.id.toString -> "A-1".asJson, urg.id.toString -> "高".asJson))).asJson)
      _         = assertEquals(ok._1, Status.Created)
      k         = ok._2.hcursor.get[String]("key").toOption.get
      // エージェントが社内の項目を入れても、"ユーザ"にはフォームの項目だけを見せる
      t        <- env.tasks.coreByKey(k).map(_.get)
      _        <- env.call(at, Method.PATCH, s"/api/tasks/$k", Some(Json.obj("fields" -> Json.obj(cost.id.toString -> "10万".asJson))))
      det      <- env.get(rt, s"/api/help/tasks/$k").map(_._2.as[HelpTaskDetailDto].toOption.get)
      _         = assertEquals(det.fields, List(HelpFieldValueDto("PC の番号", "A-1".asJson), HelpFieldValueDto("緊急度", "高".asJson)))
      ag       <- env.get(at, s"/api/tasks/${t.id}").map(_._2.as[TaskDetailDto].toOption.get.fields)
      _         = assertEquals(ag.get(cost.id.toString), Some("10万".asJson))
    yield ()
  }

  withEnv("新しい問い合わせの内容に貼った画像: 送信前は本人だけが見られ、送信すると問い合わせの添付になる") { env =>
    val png = Array[Byte](-119, 80, 78, 71, 13, 10, 26, 10, 1, 2, 3)
    def imgDoc(src: String) = Json.obj("type" -> "doc".asJson, "content" -> Json.arr(
      Json.obj("type" -> "paragraph".asJson, "content" -> Json.arr(Json.obj("type" -> "text".asJson, "text" -> "画面です".asJson))),
      Json.obj("type" -> "image".asJson, "attrs" -> Json.obj("src" -> src.asJson, "alt" -> "画面".asJson))))
    for
      a        <- env.user("エージェント")
      (agent, at) = a
      r1       <- env.user("ユーザ 一郎")
      (u1, t1)  = r1
      r2       <- env.user("ユーザ 二郎")
      (u2, t2)  = r2
      key       = "U" + UUID.randomUUID().toString.filter(_.isLetterOrDigit).toUpperCase.take(8)
      pid      <- env.projects.create(key, "添付", "", agent.id).map(_.toOption.get)
      _        <- env.put(at, s"/api/projects/$pid/helpdesk", HelpdeskSettingsReq(true, false, Nil).asJson)
      _        <- List(u1, u2).traverse_(u => env.put(at, s"/api/projects/$pid/members", SetMemberReq(u.id, Roles.Requester).asJson))
      up       <- env.upload(t1, s"/api/help/projects/$pid/uploads", "画面.png", MediaType.image.png, png)
      _         = assertEquals(up._1, Status.Created)
      url       = up._2.hcursor.get[String]("url").toOption.get
      pidUp     = up._2.hcursor.get[UUID]("id").toOption.get
      _         = assertEquals(url, s"/api/help/uploads/$pidUp")
      _        <- env.get(t1, url).map(r => assertEquals(r._1, Status.Ok, "置いた本人は見られる"))
      _        <- env.get(t2, url).map(r => assertEquals(r._1, Status.NotFound, "ほかの人には見えない"))
      // ほかの人の一時置き場の URL を貼っても、その人の問い合わせの添付にはならない
      steal    <- env.post(t2, s"/api/help/projects/$pid/tasks", HelpRequestReq("横取り", Some(imgDoc(url))).asJson)
      sk        = steal._2.hcursor.get[String]("key").toOption.get
      _        <- env.get(t2, s"/api/help/tasks/$sk").map(r => assertEquals(r._2.hcursor.downField("attachments").as[List[Json]].toOption.map(_.size), Some(0)))
      // 送信すると添付になり、内容の画像は添付の URL になる
      cr       <- env.post(t1, s"/api/help/projects/$pid/tasks", HelpRequestReq("画面が崩れる", Some(imgDoc(url))).asJson)
      _         = assertEquals(cr._1, Status.Created)
      k         = cr._2.hcursor.get[String]("key").toOption.get
      det      <- env.get(t1, s"/api/help/tasks/$k").map(_._2.as[HelpTaskDetailDto].toOption.get)
      _         = assertEquals(det.attachments.map(_.id), List(pidUp))
      _         = assert(det.descriptionHtml.contains(s"/api/help/attachments/$pidUp"), det.descriptionHtml)
      _        <- env.get(t1, s"/api/help/attachments/$pidUp").map(r => assertEquals(r._1, Status.Ok))
      _        <- env.get(t1, url).map(r => assertEquals(r._1, Status.NotFound, "一時置き場からは消える"))
      t        <- env.tasks.coreByKey(k).map(_.get)
      _         = assert(t.description.exists(_.noSpaces.contains(s"/api/attachments/$pidUp")), "エージェントの画面では添付の URL")
      _        <- env.get(at, s"/api/attachments/$pidUp").map(r => assertEquals(r._1, Status.Ok))
      _        <- env.get(t2, s"/api/help/attachments/$pidUp").map(r => assertEquals(r._1, Status.NotFound, "見えない問い合わせの添付は見えない"))
    yield ()
  }
