package skea.web

import io.circe.Json
import io.circe.parser.parse
import io.github.ondeoma.scalactive.components.ComponentManager.*
import org.scalajs.dom
import skea.shared.*

import java.util.UUID
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.util.{Failure, Success}

final case class Toast(id: Int, text: String, link: Option[String], error: Boolean)

/** 画面全体で共有する状態と、変更の自動同期（SSE で知らせを受けて取りに行く. つながらないときは15秒ごと）. */
object Store {

  val me: RV[Option[MeDto]] = RV(None)
  val config: RV[Option[ClientConfigDto]] = RV(None)
  val projects: RV[List[ProjectDto]] = RV(Nil)
  val unread: RV[UnreadDto] = RV(UnreadDto(0, 0))
  val toasts: RV[List[Toast]] = RV(Nil)

  /** 表示中のプロジェクトの設定（ステータス・ラベル・スプリント・メンバーなど）. */
  val meta: RV[Option[ProjectMetaDto]] = RV(None)

  /** 表示中のプロジェクトに変更があるたびに増える. 各画面はこれを監視して表示を取り直す. */
  val tick: RV[Int] = RV(0)

  /** タスクの作成ダイアログ（開いているときは作成先のプロジェクトと親タスク）. */
  val createDialog: RV[Option[(UUID, Option[UUID])]] = RV(None)

  private var since: Long = 0L
  private var toastSeq = 0
  private var audienceCache: Map[UUID, List[UserBrief]] = Map.empty
  private var groupCache: Map[UUID, List[MentionGroupDto]] = Map.empty

  def myId: Option[UUID] = me.v.map(_.id)

  /** 表示中のプロジェクトの ID（プロジェクトの画面の中でだけ使う）. */
  def pid: UUID = meta.v.get.project.id

  /** 表示中のプロジェクトを編集できるか・管理できるか. */
  def canEdit = meta.mapC(_.exists(m => Roles.rank(m.project.role) >= 2))
  def isProjectAdmin = meta.mapC(_.exists(_.project.role == Roles.Admin))

  /** システム管理者か. */
  def isAdmin = me.mapC(_.exists(_.isAdmin))

  // ---------------------------------------------------------------- トースト

  def toast(text: String, link: Option[String] = None, error: Boolean = false): Unit = {
    toastSeq += 1
    val t = Toast(toastSeq, text, link, error)
    toasts := (toasts.v :+ t).takeRight(5)
    dom.window.setTimeout(() => dismiss(t.id), if (error) 9000 else 6000)
  }

  def dismiss(id: Int): Unit = toasts := toasts.v.filterNot(_.id == id)

  def error(e: Throwable): Unit = e match {
    case ApiError(401, _) => ()
    case other            => toast(other.getMessage, error = true)
  }

  /** Future の失敗をトーストで知らせる. */
  def report[A](f: Future[A])(ok: A => Unit): Unit = f.onComplete {
    case Success(a) => ok(a)
    case Failure(e) => error(e)
  }

  // ---------------------------------------------------------------- 読み込み

  def loadProjects(): Future[List[ProjectDto]] = Api.projects().map { ps => projects := ps; ps }

  /** プロジェクトを表示する（設定を読み、変更の監視を始める）. */
  def openProject(key: String): Future[ProjectMetaDto] =
    meta.v.filter(_.project.key == key) match {
      case Some(m) => Future.successful(m)
      case None =>
        Api.projectByKey(key).flatMap(p => Api.meta(p.id)).map { m =>
          meta := Some(m)
          since = m.latestEventId
          connect()
          m
        }
    }

  /** 設定を読み直す（ステータスやスプリントを変えたとき）. */
  def reloadMeta(): Future[Unit] = meta.v match {
    case Some(m) => Api.meta(m.project.id).map(x => meta := Some(x))
    case None    => Future.successful(())
  }

  /** 自分の情報を読み直す（アイコンを変えたときなど. メンバーの一覧の控えも捨てる）. */
  def reloadMe(): Unit = {
    audienceCache = Map.empty
    Api.me().foreach(m => me := Some(m))
    reloadMeta()
  }

  /** まとめてメンションできるグループ・役割. */
  def mentionGroups(pid: UUID): Future[List[MentionGroupDto]] =
    groupCache.get(pid) match {
      case Some(a) => Future.successful(a)
      case None    => Api.get[List[MentionGroupDto]](s"/api/projects/$pid/mention-groups").map { a => groupCache = groupCache.updated(pid, a); a }
    }

  def audience(pid: UUID): Future[List[UserBrief]] =
    audienceCache.get(pid) match {
      case Some(a) => Future.successful(a)
      case None    => Api.audience(pid).map { a => audienceCache = audienceCache.updated(pid, a); a }
    }

  // ---------------------------------------------------------------- 自動同期

  /** 「山田 さんが SKEA-1 を作成しました」. */
  def describe(e: EventDto): String = s"${actorName(e)} さんが ${EventText.describe(e.kind, e.detail, e.taskKey)}"

  def actorName(e: EventDto): String =
    e.actor.map(_.name).getOrElse(if (e.via == "scm") e.detail.hcursor.get[String]("author").getOrElse("リポジトリ") else "skea")

  /** 変更を確認する. 自分以外（または自動化・API）の変更はトーストで知らせる. */
  def poll(): Unit = if (me.v.isDefined) {
    lastPoll = js.Date.now()
    meta.v.foreach { m =>
      Api.changes(m.project.id, since).foreach { c =>
        if (c.events.nonEmpty) {
          since = c.latest
          val notable = c.events.filterNot(e => e.kind == "task.ranked" || e.kind == "comment.reacted" || e.kind == EventKind.TaskUpdated && c.events.exists(x =>
            x.kind == EventKind.StatusChanged && x.taskId == e.taskId))
          val others = notable.filterNot(e => e.actor.map(_.id) == myId && e.via == "web")
          others.takeRight(3).foreach(e => toast(describe(e), e.taskKey.map(k => s"/tasks/$k")))
          if (c.events.exists(e => e.kind.startsWith("sprint.") || e.kind.startsWith("project."))) reloadMeta()
          tick := tick.v + 1
        }
      }
    }
    Api.unread().foreach(u => if (u != unread.v) unread := u)
  }

  /** 自分の操作の直後（他の画面の表示も取り直す）. */
  def changed(): Unit = dom.window.setTimeout(() => poll(), 300)

  // ---------------------------------------------------------------- 即時の反映（SSE）

  private var stream: Option[dom.EventSource] = None
  private var streamFor: Option[UUID] = None
  private var pending: Option[Int] = None
  private var lastPoll = 0.0

  /** 知らせが続けて届いてもまとめて1回だけ取りに行く. */
  private def soon(): Unit = if (pending.isEmpty) pending = Some(dom.window.setTimeout(() => { pending = None; poll() }, 250))

  private def live: Boolean = stream.exists(_.readyState == dom.EventSource.OPEN)

  /** 表示中のプロジェクトの変更と、自分への通知の知らせを受ける（プロジェクトが変わったらつなぎ直す）. */
  def connect(): Unit = if (me.v.isDefined) {
    val pid = meta.v.map(_.project.id)
    if (stream.isEmpty || streamFor != pid) {
      stream.foreach(_.close())
      val es = new dom.EventSource("/api/stream" + pid.fold("")(p => s"?projectId=$p"))
      es.addEventListener("change", (_: dom.MessageEvent) => soon())
      es.addEventListener("notify", (_: dom.MessageEvent) => soon())
      stream = Some(es)
      streamFor = pid
    }
  }

  def startSync(): Unit = {
    // SSE でつながっている間は、念のため60秒ごとにだけ確認する
    dom.window.setInterval(() => if (dom.document.visibilityState.toString == "visible" && (!live || js.Date.now() - lastPoll > 60000)) poll(), 15000)
    dom.document.addEventListener("visibilitychange", (_: dom.Event) => if (dom.document.visibilityState.toString == "visible") poll())
  }

}

/** DOM のイベント・クリップボードの補助. */
object Dom {

  /** 入力欄（input・select・textarea）の値. */
  def value(e: dom.Event): String = e.target.asInstanceOf[js.Dynamic].value.asInstanceOf[String]

  def checked(e: dom.Event): Boolean = e.target.asInstanceOf[dom.HTMLInputElement].checked

  def copy(text: String, message: String = "コピーしました"): Unit = {
    dom.window.navigator.clipboard.writeText(text)
    Store.toast(message)
  }

}

/** 表示の補助. */
object Fmt {

  private def opts(o: (String, String)*): js.Dynamic = js.Dynamic.literal(o.map((k, v) => k -> (v: js.Any))*)

  def dateTime(t: Timestamp): String =
    t.toDate.asInstanceOf[js.Dynamic].toLocaleString("ja-JP", opts("year" -> "numeric", "month" -> "2-digit", "day" -> "2-digit",
      "hour" -> "2-digit", "minute" -> "2-digit")).asInstanceOf[String]

  /** 「3分前」「昨日」など. */
  def ago(t: Timestamp): String = {
    val diff = (js.Date.now() - t.toEpochMilli) / 1000
    if (diff < 60) "たった今"
    else if (diff < 3600) s"${(diff / 60).toInt}分前"
    else if (diff < 86400) s"${(diff / 3600).toInt}時間前"
    else if (diff < 86400 * 7) s"${(diff / 86400).toInt}日前"
    else dateTime(t).take(10)
  }

  /** 2026-11-15 → 11/15(日). */
  def day(d: String): String = {
    val dt = new js.Date(d + "T00:00:00")
    val w  = "日月火水木金土".charAt(dt.getDay().toInt)
    s"${dt.getMonth().toInt + 1}/${dt.getDate().toInt}($w)"
  }

  def today(): String = isoDate(new js.Date())

  /** 今の時刻（13:05）. */
  def clock(): String = { val d = new js.Date(); f"${d.getHours().toInt}%02d:${d.getMinutes().toInt}%02d" }

  def isoDate(d: js.Date): String = f"${d.getFullYear().toInt}%04d-${d.getMonth().toInt + 1}%02d-${d.getDate().toInt}%02d"

  def addDays(d: String, n: Int): String = {
    val dt = new js.Date(d + "T00:00:00")
    dt.setDate(dt.getDate() + n)
    isoDate(dt)
  }

  def isOverdue(due: Option[String], done: Boolean): Boolean = !done && due.exists(_ < today())

  def initials(name: String): String = name.trim.take(1)

  def bytes(n: Long): String =
    if (n < 1024) s"$n B" else if (n < 1024 * 1024) f"${n / 1024.0}%.1f KB" else f"${n / 1024.0 / 1024.0}%.1f MB"

  def statusName(m: Option[ProjectMetaDto], id: UUID): String = m.flatMap(_.statuses.find(_.id == id)).map(_.name).getOrElse("")

  def statusCat(m: Option[ProjectMetaDto], id: UUID): String = m.flatMap(_.statuses.find(_.id == id)).map(_.category).getOrElse("")

}

/** エディタ（frontend/src/editor.js の Tiptap）. */
@js.native
@JSGlobal("skeaEditor")
private object EditorFacade extends js.Object {
  def create(el: dom.Element, opts: js.Object): EditorHandle = js.native
}

@js.native
trait EditorHandle extends js.Object {
  def getJSON(): js.Any = js.native
  def setJSON(j: js.Any): Unit = js.native
  def isEmpty(): Boolean = js.native
  def clear(): Unit = js.native
  def focus(): Unit = js.native
  def destroy(): Unit = js.native
}

object Editor {

  def toJs(j: Json): js.Any = js.JSON.parse(j.noSpaces)

  def fromJs(a: js.Any): Json = parse(js.JSON.stringify(a)).getOrElse(Json.Null)

  /**
   * エディタを作る. projectId を渡すと @メンションの候補をそのプロジェクトのメンバーから出す.
   * taskId を渡すと画像・ファイルの貼り付けでタスクに添付する.
   */
  def create(el: dom.Element, content: Option[Json], placeholder: String, projectId: Option[UUID], taskId: Option[UUID],
             onSubmit: () => Unit, autofocus: Boolean = false, onChange: Json => Unit = _ => ()): EditorHandle = {
    // メンションの候補: 人と、まとめて届くグループ・役割（「グループ・3人」のように出す）
    val mentions: js.Function1[String, js.Promise[js.Array[js.Object]]] = (q: String) => {
      import scala.scalajs.js.JSConverters.*
      val ql = q.toLowerCase
      val people = projectId.fold(Future.successful(List.empty[UserBrief]))(Store.audience)
      val groups = projectId.fold(Future.successful(List.empty[MentionGroupDto]))(p => Store.mentionGroups(p).recover(_ => Nil))
      people.zip(groups).map { (us, gs) =>
        (us.filter(u => ql.isEmpty || u.name.toLowerCase.contains(ql)).map(u => js.Dynamic.literal(id = u.id.toString, label = u.name)) ++
          gs.filter(g => ql.isEmpty || g.name.toLowerCase.contains(ql)).map(g => js.Dynamic.literal(id = g.id, label = g.name, icon = "👥",
            hint = s"${if (g.id.startsWith("r:")) "役割" else "グループ"}・${g.count}人")))
          .map(_.asInstanceOf[js.Object]).toJSArray
      }.toJSPromise
    }
    val upload: js.Any = taskId match {
      case None => null
      case Some(tid) =>
        import scala.scalajs.js.JSConverters.*
        val f: js.Function1[dom.File, js.Promise[js.Object]] = (file: dom.File) =>
          Api.upload(tid, file).map(a => js.Dynamic.literal(url = a.url, name = a.fileName, image = a.contentType.startsWith("image/")).asInstanceOf[js.Object]).toJSPromise
        f
    }
    val o = js.Dynamic.literal(
      content = content.map(toJs).getOrElse(null),
      placeholder = placeholder,
      mentions = mentions,
      upload = upload,
      autofocus = autofocus,
      onSubmit = (() => onSubmit()): js.Function0[Unit],
      onChange = ((j: js.Any) => onChange(fromJs(j))): js.Function1[js.Any, Unit]
    )
    EditorFacade.create(el, o.asInstanceOf[js.Object])
  }

}

/** ドラッグ中の一覧の行: タスクか、落とす位置の空き（gap）. */
final case class DndRow(task: Option[TaskDto]) {
  def key: String = task.map(_.id.toString).getOrElse("gap")
  def id: String = task.map(_.id.toString).getOrElse("")
}

/**
 * カンバン・バックログのドラッグ＆ドロップ（タスクの移動）.
 * ドラッグ中は元のカードを隠し、落とす位置に空きを入れて、ほかのカードがずれる様子を動きで見せる.
 * 区分（zone）はカンバンならステータス、バックログならスプリント.
 */
object Dnd {

  final case class Drag(id: UUID, zone: String, height: Double)

  val drag: RV[Option[Drag]] = RV(None)
  /** 隠している元のカード（ドラッグの開始の直後に隠す. すぐに隠すとブラウザがドラッグをやめるため）. */
  val hidden: RV[Option[UUID]] = RV(None)
  /** 落とす位置（区分と、その前に入れるタスク. None は末尾）. */
  val target: RV[Option[(String, Option[UUID])]] = RV(None)
  private var droppedAt = 0.0

  def start(t: TaskDto, zone: String, list: List[TaskDto], e: dom.Event): Unit = {
    val de = e.asInstanceOf[dom.DragEvent]
    de.dataTransfer.setData("text/plain", t.key)
    de.dataTransfer.effectAllowed = dom.DataTransferEffectAllowedKind.move
    val h = e.currentTarget.asInstanceOf[dom.HTMLElement].getBoundingClientRect().height
    drag := Some(Drag(t.id, zone, h))
    val next = list.dropWhile(_.id != t.id).drop(1).headOption.map(_.id)
    dom.window.setTimeout(() => if (drag.v.exists(_.id == t.id)) Flip.animate { hidden := Some(t.id); target := Some((zone, next)) }, 0)
  }

  /** 区分の一覧の行（落とす位置に空きを入れる）. computed の中で呼ぶこと. */
  def rows(zone: String, list: List[TaskDto]): List[DndRow] = {
    val base = list.map(t => DndRow(Some(t)))
    (target.v, hidden.v) match {
      case (Some((z, before)), Some(_)) if z == zone =>
        val i = before.map(b => list.indexWhere(_.id == b)).filter(_ >= 0).getOrElse(list.size)
        base.take(i) ++ List(DndRow(None)) ++ base.drop(i)
      case _ => base
    }
  }

  /** 区分の上を通るとき: ポインタより下にある最初のカードの前を落とす位置にする. */
  def over(zone: String, e: dom.Event): Unit = drag.v.foreach { d =>
    e.preventDefault()
    val y = e.asInstanceOf[dom.DragEvent].clientY
    val cards = e.currentTarget.asInstanceOf[dom.Element].querySelectorAll("[data-task]").toList
      .map(_.asInstanceOf[dom.HTMLElement]).filter(c => c.getAttribute("data-task").nonEmpty && c.getAttribute("data-task") != d.id.toString)
    val before = cards.find { c => val r = c.getBoundingClientRect(); r.top + r.height / 2 > y }.map(c => UUID.fromString(c.getAttribute("data-task")))
    val nt = Some((zone, before))
    if (target.v != nt && hidden.v.isDefined) Flip.animate(target := nt)
  }

  /** 落としたとき: (タスク, 後ろに来るタスク, 前に来るタスク). list は落とした区分のタスク. */
  def drop(zone: String, list: List[TaskDto], e: dom.Event): Option[(UUID, Option[UUID], Option[UUID])] = {
    e.preventDefault()
    val r = for {
      d <- drag.v
      t <- target.v if t._1 == zone
    } yield {
      val others = list.filterNot(_.id == d.id)
      val i      = t._2.map(b => others.indexWhere(_.id == b)).filter(_ >= 0).getOrElse(others.size)
      (d.id, if (i > 0) Some(others(i - 1).id) else None, t._2)
    }
    droppedAt = js.Date.now()
    Flip.animate(end())
    r
  }

  /** ドラッグの終わり（落とさなかったときは元に戻る）. */
  def end(): Unit = { drag := None; hidden := None; target := None }

  /** 落とした直後のクリックは無視する（ドラッグをクリックとして扱わない）. */
  def recentlyDropped: Boolean = js.Date.now() - droppedAt < 400

  def cls(r: DndRow) = r.task match {
    case None    => hidden.mapC(_ => "dnd-gap")
    case Some(t) => hidden.mapC(h => if (h.contains(t.id)) "dnd-item dnd-src" else "dnd-item")
  }

  def zoneClass(zone: String, base: String) = target.mapC(t => if (t.exists(_._1 == zone)) s"$base over" else base)

  def gapStyle(r: DndRow): String = if (r.task.isEmpty) s"height: ${drag.v.map(_.height).getOrElse(40.0)}px" else ""

  /** 画面の一覧の中でタスクを動かす（サーバーの応答を待たずに表示を変える）. */
  def reinsert(tasks: List[TaskDto], id: UUID, after: Option[UUID], before: Option[UUID])(update: TaskDto => TaskDto): List[TaskDto] = {
    val moved = tasks.find(_.id == id).map(update)
    val rest  = tasks.filterNot(_.id == id)
    val idx   = before.map(b => rest.indexWhere(_.id == b)).filter(_ >= 0).getOrElse(after.map(a => rest.indexWhere(_.id == a) + 1).getOrElse(rest.size))
    rest.take(idx) ++ moved.toList ++ rest.drop(idx)
  }

}

/** 一覧の並びが変わるとき、カード（data-task）を元の位置から新しい位置へ滑らかに動かす（FLIP）. */
object Flip {

  private def cards(): Map[String, dom.HTMLElement] =
    dom.document.querySelectorAll("[data-task]").toList.map(_.asInstanceOf[dom.HTMLElement])
      .filter(_.getAttribute("data-task").nonEmpty).map(e => e.getAttribute("data-task") -> e).toMap

  def animate(change: => Unit): Unit = {
    val before = cards().view.mapValues(_.getBoundingClientRect()).toMap
    change
    cards().foreach { (k, el) =>
      before.get(k).filter(_.width > 0).foreach { r0 =>
        val r1 = el.getBoundingClientRect()
        val dx = r0.left - r1.left
        val dy = r0.top - r1.top
        if (r1.width > 0 && (math.abs(dx) > 1 || math.abs(dy) > 1))
          el.asInstanceOf[js.Dynamic].animate(
            js.Array(js.Dynamic.literal(transform = s"translate(${dx}px, ${dy}px)"), js.Dynamic.literal(transform = "none")),
            js.Dynamic.literal(duration = 160, easing = "ease-out"))
      }
    }
  }

}

/** 画面内のダイアログ（ブラウザ標準の confirm・prompt・alert の代わり）. 表示は parts/DialogHost.st. */
final case class DialogReq(message: String, okLabel: String, danger: Boolean, input: Option[RV[String]], choices: List[(String, String)],
                           cancel: Boolean, onOk: String => Unit, onCancel: () => Unit)

object Dialog {

  val current: RV[Option[DialogReq]] = RV(None)

  private def open(r: DialogReq): Unit = current := Some(r)

  /** 確認（OK を押したら f）. */
  def confirm(message: String, ok: String = "OK", danger: Boolean = true)(f: => Unit): Unit =
    open(DialogReq(message, ok, danger, None, Nil, cancel = true, _ => f, () => ()))

  /** 文字の入力（OK を押したら入力した値で f）. */
  def prompt(message: String, default: String = "", ok: String = "OK")(f: String => Unit): Unit =
    open(DialogReq(message, ok, danger = false, Some(RV(default)), Nil, cancel = true, f, () => ()))

  /** 選択肢から選ぶ（choices は 値 → 表示名）. */
  def choose(message: String, choices: List[(String, String)], ok: String = "OK", danger: Boolean = false)(f: String => Unit): Unit =
    open(DialogReq(message, ok, danger, Some(RV(choices.headOption.map(_._1).getOrElse(""))), choices, cancel = true, f, () => ()))

  def alert(message: String): Unit = open(DialogReq(message, "OK", danger = false, None, Nil, cancel = false, _ => (), () => ()))

  def close(ok: Boolean): Unit = current.v.foreach { r =>
    current := None
    if (ok) r.onOk(r.input.map(_.v).getOrElse("")) else r.onCancel()
  }

  /** エディタ（JavaScript）から使う: window.skeaUi.prompt(message, default) → Promise(文字列 | null)、alert(message). */
  def exposeToJs(): Unit = {
    val prompt: js.Function2[String, String, js.Promise[js.Any]] = (message: String, default: String) =>
      new js.Promise[js.Any]((resolve, _) =>
        open(DialogReq(message, "OK", danger = false, Some(RV(Option(default).getOrElse(""))), Nil, cancel = true,
          v => resolve(v), () => resolve(null))))
    val alertF: js.Function1[String, Unit] = (m: String) => alert(m)
    dom.window.asInstanceOf[js.Dynamic].skeaUi = js.Dynamic.literal(prompt = prompt, alert = alertF)
  }

}

/** この端末だけの表示の設定（localStorage. 使えないときは既定値）. */
object Prefs {

  def get(key: String, default: String): String =
    scala.util.Try(Option(dom.window.localStorage.getItem("skea." + key))).toOption.flatten.getOrElse(default)

  def set(key: String, value: String): Unit = scala.util.Try(dom.window.localStorage.setItem("skea." + key, value))

}

/**
 * タッチ端末でのドラッグ＆ドロップ（カンバン・バックログ）.
 * HTML のドラッグ＆ドロップはタッチでは動かないため、カードを長押しするとドラッグを始め、指の位置にドラッグのイベント
 * （dragstart / dragover / drop / dragend）を送って、マウスと同じ処理で動かす. 指の下にはカードの複製を出す.
 */
object TouchDnd {

  private val LongPress = 350.0
  private var timer: Option[Int] = None
  private var source: Option[dom.HTMLElement] = None
  private var ghost: Option[dom.HTMLElement] = None
  private var dt: js.Dynamic = null
  private var start = (0.0, 0.0)
  private var last = (0.0, 0.0)
  private var offset = (0.0, 0.0)
  private var scroller: Option[Int] = None
  private var suppressClickUntil = 0.0

  private def drag(kind: String, target: dom.EventTarget, x: Double, y: Double): Unit = {
    val init = js.Dynamic.literal(bubbles = true, cancelable = true, dataTransfer = dt, clientX = x, clientY = y)
    target.dispatchEvent(js.Dynamic.newInstance(js.Dynamic.global.DragEvent)(kind, init).asInstanceOf[dom.Event])
  }

  private def under(x: Double, y: Double): Option[dom.Element] = Option(dom.document.elementFromPoint(x, y))

  private def cancelTimer(): Unit = { timer.foreach(dom.window.clearTimeout); timer = None }

  private def begin(el: dom.HTMLElement): Unit = {
    dt = js.Dynamic.newInstance(js.Dynamic.global.DataTransfer)()
    val r = el.getBoundingClientRect()
    offset = (start._1 - r.left, start._2 - r.top)
    val g = el.cloneNode(true).asInstanceOf[dom.HTMLElement]
    g.classList.add("touch-ghost")
    g.style.width = s"${r.width}px"
    g.style.left = s"${r.left}px"
    g.style.top = s"${r.top}px"
    dom.document.body.appendChild(g)
    ghost = Some(g)
    source = Some(el)
    drag("dragstart", el, start._1, start._2)
    scala.util.Try(dom.window.navigator.asInstanceOf[js.Dynamic].vibrate(20))
    // 画面の端に近いときは一覧を自動でスクロールする
    scroller = Some(dom.window.setInterval(() => autoScroll(), 30))
  }

  private def autoScroll(): Unit = {
    val (x, y) = last
    val edge = 40.0
    val vx = if (x < edge) -12 else if (x > dom.window.innerWidth - edge) 12 else 0
    val vy = if (y < edge + 50) -12 else if (y > dom.window.innerHeight - edge) 12 else 0
    if (vy != 0) dom.window.scrollBy(0, vy)
    if (vx != 0) under(x, y).flatMap(e => Option(e.closest(".board, .cols"))).foreach(b => b.scrollLeft = b.scrollLeft + vx)
  }

  private def finish(drop: Boolean): Unit = {
    cancelTimer()
    scroller.foreach(dom.window.clearInterval); scroller = None
    ghost.foreach(_.remove()); ghost = None
    source.foreach { el =>
      if (drop) under(last._1, last._2).foreach(t => drag("drop", t, last._1, last._2))
      drag("dragend", el, last._1, last._2)
      suppressClickUntil = js.Date.now() + 500
    }
    source = None
  }

  def install(): Unit = {
    dom.document.addEventListener("touchstart", (e: dom.TouchEvent) => {
      val card = Option(e.target.asInstanceOf[dom.Element].closest("[draggable=\"true\"][data-task]")).map(_.asInstanceOf[dom.HTMLElement])
      card.filter(_ => e.touches.length == 1).foreach { el =>
        val t = e.touches(0)
        start = (t.clientX, t.clientY)
        last = start
        cancelTimer()
        timer = Some(dom.window.setTimeout(() => { timer = None; begin(el) }, LongPress))
      }
    }, js.Dynamic.literal(passive = true).asInstanceOf[dom.EventListenerOptions])

    dom.document.addEventListener("touchmove", (e: dom.TouchEvent) => {
      val t = e.touches(0)
      last = (t.clientX, t.clientY)
      if (source.isDefined) {
        e.preventDefault()
        ghost.foreach { g => g.style.left = s"${t.clientX - offset._1}px"; g.style.top = s"${t.clientY - offset._2}px" }
        under(t.clientX, t.clientY).foreach(el => drag("dragover", el, t.clientX, t.clientY))
      } else if (math.abs(t.clientX - start._1) > 8 || math.abs(t.clientY - start._2) > 8) cancelTimer() // スクロールしている
    }, js.Dynamic.literal(passive = false).asInstanceOf[dom.EventListenerOptions])

    dom.document.addEventListener("touchend", (_: dom.TouchEvent) => finish(drop = true))
    dom.document.addEventListener("touchcancel", (_: dom.TouchEvent) => finish(drop = false))
    // 長押しで出るメニューと、ドラッグの直後のクリックを止める
    dom.document.addEventListener("contextmenu", (e: dom.Event) => if (source.isDefined || timer.isDefined) e.preventDefault())
    dom.document.addEventListener("click", (e: dom.Event) => if (js.Date.now() < suppressClickUntil) { e.preventDefault(); e.stopPropagation() }, true)
  }

}

/** 画面の色（自動 = OS の設定、ライト、ダーク）. この端末に記憶する. */
object Theme {

  val current: RV[String] = RV(Prefs.get("theme", "system"))

  def apply(): Unit = {
    val el = dom.document.documentElement
    if (current.v == "system") el.removeAttribute("data-theme") else el.setAttribute("data-theme", current.v)
  }

  def set(v: String): Unit = { current := v; Prefs.set("theme", v); apply() }

  val options: List[(String, String)] = List("system" -> "🖥 自動（OS に合わせる）", "light" -> "☀️ ライト", "dark" -> "🌙 ダーク")

}

/** オートコンプリート（parts/Combo.st）の選択肢. icon は前に付ける文字、user はアイコン、color はラベルの色. */
final case class Opt(value: String, label: String, icon: String = "", user: Option[UserBrief] = None, color: String = "", hint: String = "")

object Opt {
  def users(us: List[UserBrief]): List[Opt] = us.map(u => Opt(u.id.toString, u.name, user = Some(u)))
  def of(xs: List[(String, String)]): List[Opt] = xs.map((v, l) => Opt(v, l))
  /** 値と表示が同じ選択肢. */
  def names(xs: List[String]): List[Opt] = xs.map(x => Opt(x, x))
  def statuses(ss: List[StatusDto]): List[Opt] = ss.map(s => Opt(s.id.toString, s.name, color = s.color))
  val priorities: List[Opt] = Priority.all.map(p => Opt(p, Priority.label(p), Priority.icon(p)))
  val types: List[Opt] = TaskType.all.map(t => Opt(t, TaskType.label(t), TaskType.icon(t)))
}

/** カスタム絵文字（リアクション用. 一度だけ読む）. */
object Emojis {

  val custom: RV[List[CustomEmojiDto]] = RV(Nil)
  private var loaded = false

  def load(force: Boolean = false): Unit = if (!loaded || force) {
    loaded = true
    Api.get[List[CustomEmojiDto]]("/api/emoji").foreach(custom := _)
  }

  /** ":name:" ならカスタム絵文字の画像の URL. */
  def image(e: String): Option[String] = Reactions.customName(e).map(CustomEmoji.url)

}

/** 外部サービス（skiwi・skawau）へのリンクに名前を出す（frontend/src/links.js. 連携したサービスだけ動かす）. */
object LinkTitles {

  private var handles = Map.empty[String, js.Dynamic]

  def start(svc: LinkServiceDto): Unit = if (svc.connected) handles.get(svc.provider) match {
    case Some(h) => h.reset()
    case None    => handles += svc.provider -> dom.window.asInstanceOf[js.Dynamic].skeaLinks.start(svc.provider, svc.url)
  }

  /** 連携しているサービスを問い合わせて動かす. */
  def load(): scala.concurrent.Future[List[LinkServiceDto]] = {
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
    val f = Api.get[List[LinkServiceDto]]("/api/integrations/links")
    f.foreach(_.foreach(start))
    f
  }

}
