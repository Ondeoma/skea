<script lang="scala">
import skea.web.*
import io.circe.Json
import io.circe.syntax.*
import org.scalajs.dom
import skea.shared.*
import skea.web.parts.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

// ガントチャート: 親子の階層・期間のバー（ドラッグで移動・伸縮）・依存関係の矢印・マイルストーン.
// バーの右端の丸から別のバーへドラッグすると依存関係（先行 → 後続）をつなぎ、矢印をクリックすると外せる.

/** 依存関係をつないでいる途中（先行タスクと、線の始点・終点）. */
final case class Link(from: TaskDto, x1: Double, y1: Double, x2: Double, y2: Double)

case class State(data: RV[Option[GanttDto]] = RV(None),
                 done: RV[Boolean] = RV(false),
                 dayW: RV[Int] = RV(28),
                 collapsed: RV[Set[UUID]] = RV(Set.empty),
                 preview: RV[Map[UUID, (String, String)]] = RV(Map.empty),
                 linking: RV[Option[Link]] = RV(None),
                 msEdit: RV[Option[Option[MilestoneDto]]] = RV(None),
                 msName: RV[String] = RV(""),
                 msDate: RV[String] = RV(""),
                 msColor: RV[String] = RV("#dc2626"),
                 msPreview: RV[Map[UUID, String]] = RV(Map.empty),
                 dating: RV[Option[TaskDto]] = RV(None),
                 dStart: RV[String] = RV(""),
                 dDue: RV[String] = RV(""),
                 critical: RV[Boolean] = RV(Prefs.get("gantt.critical", "false") == "true"),
                 shiftNext: RV[Boolean] = RV(Prefs.get("gantt.shiftNext", "true") == "true"))

def initState() = State()

val RowH = 34

def load(s: State): Unit = Store.meta.v.foreach(m => Api.gantt(m.project.id, s.done.v).foreach(g => s.data := Some(g)))

def mounted(s: State): Unit = {
  pageTitle("ガントチャート")
  load(s)
  watch(Store.tick, false)((_, _) => if (drag.isEmpty && s.linking.v.isEmpty) load(s))
}

// ---------------------------------------------------------------- 日付の計算

private def ms(d: String): Double = js.Date.UTC(d.take(4).toInt, d.slice(5, 7).toInt - 1, d.slice(8, 10).toInt)
def diff(a: String, b: String): Int = math.round((ms(b) - ms(a)) / 86400000).toInt
def plus(d: String, n: Int): String = {
  val x = new js.Date(ms(d) + n * 86400000.0)
  f"${x.getUTCFullYear().toInt}%04d-${x.getUTCMonth().toInt + 1}%02d-${x.getUTCDate().toInt}%02d"
}

/** バーの期間（開始・終了。片方しかなければ1日）. */
def span(s: State, t: TaskDto): Option[(String, String)] =
  s.preview.v.get(t.id).orElse((t.startDate, t.dueDate) match {
    case (Some(a), Some(b)) => Some((a, if (b < a) a else b))
    case (Some(a), None)    => Some((a, a))
    case (None, Some(b))    => Some((b, b))
    case _                  => None
  })

/** 表示する期間（タスクの期間の前後に余白を取り、最低6週間）. */
def range(s: State): (String, Int) = {
  val ds = s.data.v.toList.flatMap(g => g.tasks.flatMap(t => t.startDate.toList ++ t.dueDate.toList) ++ g.milestones.map(_.date)) :+ Fmt.today()
  val from = plus(ds.min, -7)
  val to   = plus(ds.max, 21)
  (from, math.max(42, diff(from, to)))
}

// ---------------------------------------------------------------- 行（親子の順）

final case class Row(t: TaskDto, depth: Int, hasChildren: Boolean)

def rows(s: State) = computed {
  val ts = s.data.v.toList.flatMap(_.tasks)
  val ids = ts.map(_.id).toSet
  val byParent = ts.groupBy(t => t.parentId.filter(ids.contains))
  val col = s.collapsed.v
  def walk(p: Option[UUID], depth: Int): List[Row] =
    byParent.getOrElse(p, Nil).flatMap { t =>
      val kids = byParent.contains(Some(t.id))
      Row(t, depth, kids) :: (if (col.contains(t.id)) Nil else walk(Some(t.id), depth + 1))
    }
  walk(None, 0)
}

def toggle(s: State, id: UUID): Unit = s.collapsed := (if (s.collapsed.v.contains(id)) s.collapsed.v - id else s.collapsed.v + id)

// ---------------------------------------------------------------- 描画

def days(s: State) = computed {
  s.data.v
  val (from, n) = range(s)
  (0 until n).toList.map(i => plus(from, i))
}

def width(s: State) = computed { val (_, n) = range(s); s"width: ${n * s.dayW.v}px" }
def height(s: State) = rows(s).mapC(rs => s"height: ${rs.size * RowH}px")

def dayClass(d: String): String = {
  val w = new js.Date(ms(d)).getUTCDay().toInt
  (if (w == 0 || w == 6) "day weekend" else "day") + (if (d == Fmt.today()) " today" else "")
}
def dayLabel(s: State, d: String) = s.dayW.mapC(w => if (w >= 20 || d.endsWith("-01") || new js.Date(ms(d)).getUTCDay().toInt == 1) d.drop(8).toInt.toString else "")
def monthLabel(d: String): String = if (d.endsWith("-01")) s"${d.take(4)}/${d.slice(5, 7)}" else ""
def dayStyle(s: State) = s.dayW.mapC(w => s"width: ${w}px")

def barStyle(s: State, t: TaskDto) = computed {
  val (from, _) = range(s)
  span(s, t).fold("display: none") { (a, b) =>
    s"left: ${diff(from, a) * s.dayW.v}px; width: ${(diff(a, b) + 1) * s.dayW.v - 4}px"
  }
}
def barClass(t: TaskDto): String = "bar cat-" + Fmt.statusCat(Store.meta.v, t.statusId)
def todayStyle(s: State) = computed { val (from, _) = range(s); s"left: ${diff(from, Fmt.today()) * s.dayW.v + s.dayW.v / 2}px" }

/** 依存関係の矢印（先行の終わり → 後続の始まり）の SVG. */
def arrows(s: State) = computed {
  val (from, n) = range(s)
  val rs = rows(s).v
  val idx = rs.zipWithIndex.map((r, i) => r.t.id -> (r.t, i)).toMap
  val w = s.dayW.v
  val paths = s.data.v.toList.flatMap(_.dependencies).flatMap { d =>
    for {
      (p, pi) <- idx.get(d.predecessorId)
      (c, ci) <- idx.get(d.successorId)
      (_, pe) <- span(s, p)
      (cs, _) <- span(s, c)
    } yield {
      val x1 = (diff(from, pe) + 1) * w - 4
      val y1 = pi * RowH + RowH / 2
      val x2 = diff(from, cs) * w
      val y2 = ci * RowH + RowH / 2
      val late = diff(pe, cs) < 1
      val crit = criticalSet(s).v
      val onPath = crit(p.id) && crit(c.id)
      val mx = math.max(x1 + 8, x2 - 8)
      val path = s"M$x1 $y1 H$mx V$y2 H${x2 - 2}"
      val tip = s"${p.key} → ${c.key}（クリックで外す）"
      // 見える矢印と、クリックしやすい太い透明の線
      s"""<path d="$path" class="${if (late) "dep late" else "dep"}${if (onPath) " crit" else ""}" marker-end="url(#arrow)"/>""" +
        s"""<path d="$path" class="dep-hit" data-pred="${p.id}" data-succ="${c.id}"><title>${Html.escape(tip)}</title></path>"""
    }
  }
  s"""<svg width="${n * w}" height="${rs.size * RowH}"><defs><marker id="arrow" viewBox="0 0 8 8" refX="7" refY="4" markerWidth="7" markerHeight="7" orient="auto"><path d="M0 0 L8 4 L0 8 z" class="dep-head"/></marker></defs>${paths.mkString}</svg>"""
}

// ---------------------------------------------------------------- クリティカルパス・後続のタスク

/** 元の期間（ドラッグ中の仮の表示を除く）. */
def spanOf(t: TaskDto): Option[(String, String)] = (t.startDate, t.dueDate) match {
  case (Some(a), Some(b)) => Some((a, if (b < a) a else b))
  case (Some(a), None)    => Some((a, a))
  case (None, Some(b))    => Some((b, b))
  case _                  => None
}

/** 後続のタスク（依存関係をたどったすべて. 日付のあるものだけ）. */
def downstream(s: State, id: UUID): List[TaskDto] = {
  val g    = s.data.v.toList
  val deps = g.flatMap(_.dependencies)
  val byId = g.flatMap(_.tasks).map(t => t.id -> t).toMap
  @scala.annotation.tailrec
  def go(queue: List[UUID], seen: Set[UUID]): Set[UUID] = queue match {
    case Nil => seen
    case h :: rest =>
      val next = deps.filter(_.predecessorId == h).map(_.successorId).filterNot(seen)
      go(rest ++ next, seen ++ next)
  }
  (go(List(id), Set.empty) - id).toList.flatMap(byId.get).filter(t => spanOf(t).isDefined)
}

/**
 * クリティカルパス: 最も遅く終わるタスクから、終わりの最も遅い先行タスクを順にたどった列
 * （全体の終わりを決めているタスクの並び. どれかが遅れると全体が遅れる）.
 */
def criticalSet(s: State) = computed {
  if (!s.critical.v) Set.empty[UUID]
  else {
    val g    = s.data.v.toList
    val deps = g.flatMap(_.dependencies)
    val ts   = g.flatMap(_.tasks).filter(t => span(s, t).isDefined)
    val end  = (t: TaskDto) => span(s, t).get._2
    ts.sortBy(end).lastOption.fold(Set.empty[UUID]) { last =>
      @scala.annotation.tailrec
      def back(cur: TaskDto, acc: Set[UUID]): Set[UUID] = {
        val preds = deps.filter(_.successorId == cur.id).flatMap(d => ts.find(_.id == d.predecessorId)).filterNot(p => acc(p.id))
        preds.sortBy(end).lastOption match {
          case Some(p) => back(p, acc + p.id)
          case None    => acc
        }
      }
      back(last, Set(last.id))
    }
  }
}

def toggleCritical(s: State): Unit = { s.critical := !s.critical.v; Prefs.set("gantt.critical", s.critical.v.toString) }
def toggleShift(s: State): Unit = { s.shiftNext := !s.shiftNext.v; Prefs.set("gantt.shiftNext", s.shiftNext.v.toString) }
def barClassC(s: State, t: TaskDto) = criticalSet(s).mapC(c => barClass(t) + (if (c(t.id)) " crit" else ""))
def critBtn(s: State) = s.critical.mapC(c => if (c) "small active-btn" else "small")

// ---------------------------------------------------------------- ドラッグ（移動・伸縮）

final case class Drag(t: TaskDto, mode: String, x0: Double, a: String, b: String)
private var drag: Option[Drag] = None
private var moveL: js.Function1[dom.MouseEvent, Unit] = null
private var upL: js.Function1[dom.MouseEvent, Unit] = null


def down(s: State, t: TaskDto, mode: String, e: dom.Event): Unit = if (Store.canEdit.v) span(s, t).foreach { (a, b) =>
  val me = e.asInstanceOf[dom.MouseEvent]
  me.preventDefault()
  me.stopPropagation()
  drag = Some(Drag(t, mode, me.clientX, a, b))
  moveL = (ev: dom.MouseEvent) => drag.foreach { d =>
    val n = math.round((ev.clientX - d.x0) / s.dayW.v).toInt
    val (na, nb) = d.mode match {
      case "start" => (if (plus(d.a, n) > d.b) d.b else plus(d.a, n), d.b)
      case "end"   => (d.a, if (plus(d.b, n) < d.a) d.a else plus(d.b, n))
      case _       => (plus(d.a, n), plus(d.b, n))
    }
    if (!s.preview.v.get(d.t.id).contains((na, nb))) {
      // 終わりが動いたら、後続のタスクも同じ日数だけずらす（設定で切り替え）
      val delta = diff(d.b, nb)
      val next = if (s.shiftNext.v && d.mode != "start") downstream(s, d.t.id).flatMap(t => spanOf(t).map(x => t.id -> (plus(x._1, delta), plus(x._2, delta)))) else Nil
      s.preview := (s.preview.v -- downstream(s, d.t.id).map(_.id)).updated(d.t.id, (na, nb)) ++ next.filter(_ => delta != 0)
    }
  }
  upL = (_: dom.MouseEvent) => {
    dom.window.removeEventListener("pointermove", moveL)
    dom.window.removeEventListener("pointerup", upL)
    drag.foreach { d =>
      s.preview.v.get(d.t.id) match {
        case Some((na, nb)) if (na, nb) != (d.a, d.b) =>
          val body = Json.obj("startDate" -> (if (d.t.startDate.isDefined || na != nb) na.asJson else Json.Null),
            "dueDate" -> nb.asJson, "version" -> d.t.version.asJson)
          // 一緒にずらした後続のタスク（元の日付の持ち方に合わせて開始日・期日を送る）
          val others = downstream(s, d.t.id).flatMap(t => s.preview.v.get(t.id).map(p => (t, p)))
          val shifted = others.map { (t, p) =>
            Api.updateTask(t.id, Json.obj(List(
              t.startDate.map(_ => "startDate" -> p._1.asJson), t.dueDate.map(_ => "dueDate" -> p._2.asJson)).flatten*))
          }
          Api.updateTask(d.t.id, body).flatMap(_ => scala.concurrent.Future.sequence(shifted)).onComplete { r =>
            r.failed.foreach(Store.error)
            if (others.nonEmpty && r.isSuccess) Store.toast(s"後続のタスク ${others.size} 件も一緒にずらしました")
            s.preview := Map.empty
            load(s)
            Store.changed()
          }
        case _ => s.preview := Map.empty
      }
    }
    drag = None
  }
  dom.window.addEventListener("pointermove", moveL)
  dom.window.addEventListener("pointerup", upL)
}


// ---------------------------------------------------------------- 依存関係（バーの右端の丸からドラッグしてつなぐ・矢印をクリックして外す）

private def gridRect(): Option[dom.DOMRect] = Option(dom.document.querySelector(".gantt .grid")).map(_.getBoundingClientRect())

def knobStyle(s: State, t: TaskDto) = computed {
  val (from, _) = range(s)
  span(s, t).fold("display: none") { (_, b) => s"left: ${(diff(from, b) + 1) * s.dayW.v - 1}px" }
}

def linkStart(s: State, t: TaskDto, e: dom.Event): Unit = if (Store.canEdit.v) span(s, t).foreach { (_, b) =>
  val me = e.asInstanceOf[dom.MouseEvent]
  me.preventDefault()
  me.stopPropagation()
  val (from, _) = range(s)
  val i  = rows(s).v.indexWhere(_.t.id == t.id)
  val x1 = (diff(from, b) + 1) * s.dayW.v - 4.0
  val y1 = i * RowH + RowH / 2.0
  s.linking := Some(Link(t, x1, y1, x1, y1))
  moveL = (ev: dom.MouseEvent) => gridRect().foreach { g =>
    s.linking := s.linking.v.map(_.copy(x2 = ev.clientX - g.left, y2 = ev.clientY - g.top))
  }
  upL = (ev: dom.MouseEvent) => {
    dom.window.removeEventListener("pointermove", moveL)
    dom.window.removeEventListener("pointerup", upL)
    val target = Option(dom.document.elementFromPoint(ev.clientX, ev.clientY)).flatMap(el => Option(el.closest("[data-bar]")))
      .map(_.getAttribute("data-bar")).filter(_ != t.id.toString)
    s.linking := None
    target.flatMap(id => s.data.v.toList.flatMap(_.tasks).find(_.id.toString == id)).foreach { succ =>
      Store.report(Api.postUnit(s"/api/tasks/${succ.id}/dependencies", CreateDependencyReq(t.key, None).asJson)) { _ =>
        Store.toast(s"${t.key} → ${succ.key} の依存関係をつなぎました")
        load(s); Store.changed()
      }
    }
  }
  dom.window.addEventListener("pointermove", moveL)
  dom.window.addEventListener("pointerup", upL)
}

/** 矢印のクリック: 依存関係を外す. */
def arrowClick(s: State, e: dom.Event): Unit = if (Store.canEdit.v) {
  val el = e.target.asInstanceOf[dom.Element]
  (Option(el.getAttribute("data-pred")).filter(_.nonEmpty), Option(el.getAttribute("data-succ")).filter(_.nonEmpty)) match {
    case (Some(p), Some(c)) =>
      val ts  = s.data.v.toList.flatMap(_.tasks)
      val key = (id: String) => ts.find(_.id.toString == id).map(_.key).getOrElse("")
      Dialog.confirm(s"${key(p)} → ${key(c)} の依存関係を外しますか？", "外す") {
        Store.report(Api.delete(s"/api/tasks/$c/dependencies/$p")) { _ => load(s); Store.changed() }
      }
    case _ => ()
  }
}

def linkSvg(s: State) = computed {
  val (_, n) = range(s)
  s.linking.v.fold("") { l =>
    s"""<svg width="${n * s.dayW.v}" height="${rows(s).v.size * RowH}"><line x1="${l.x1}" y1="${l.y1}" x2="${l.x2}" y2="${l.y2}" class="link-line"/>""" +
      s"""<circle cx="${l.x2}" cy="${l.y2}" r="4" class="link-end"/></svg>"""
  }
}
def ganttClass(s: State) = s.linking.mapC(l => if (l.isDefined) "gantt linking" else "gantt")

// ---------------------------------------------------------------- マイルストーン

def milestones(s: State) = s.data.mapC(_.map(_.milestones).getOrElse(Nil))
def msStyle(s: State, m: MilestoneDto) = computed {
  val (from, _) = range(s)
  val d = s.msPreview.v.getOrElse(m.id, m.date)
  s"left: ${diff(from, d) * s.dayW.v + s.dayW.v / 2}px; --ms: ${m.color}"
}

/** マイルストーンのドラッグ（左右に動かして日付を変える. 動かさずに離したら編集）. */
def msDown(s: State, m: MilestoneDto, e: dom.Event): Unit = {
  val pe = e.asInstanceOf[dom.MouseEvent]
  pe.preventDefault()
  val x0 = pe.clientX
  var moved = false
  moveL = (ev: dom.MouseEvent) => {
    val n = math.round((ev.clientX - x0) / s.dayW.v).toInt
    if (n != 0) moved = true
    if (Store.canEdit.v) s.msPreview := s.msPreview.v.updated(m.id, plus(m.date, n))
  }
  upL = (_: dom.MouseEvent) => {
    dom.window.removeEventListener("pointermove", moveL)
    dom.window.removeEventListener("pointerup", upL)
    s.msPreview.v.get(m.id).filter(d => moved && d != m.date) match {
      case Some(d) => Store.meta.v.foreach { meta =>
          Api.putUnit(s"/api/projects/${meta.project.id}/milestones/${m.id}", MilestoneReq(m.name, d, Some(m.color)).asJson).onComplete { r =>
            r.failed.foreach(Store.error)
            if (r.isSuccess) Store.toast(s"「${m.name}」を ${Fmt.day(d)} に移しました")
            s.msPreview := s.msPreview.v - m.id
            load(s); Store.changed()
          }
        }
      case None => s.msPreview := s.msPreview.v - m.id; if (!moved) editMs(s, Some(m))
    }
  }
  dom.window.addEventListener("pointermove", moveL)
  dom.window.addEventListener("pointerup", upL)
}

// ---------------------------------------------------------------- 日付のないタスクに日付を入れる

def openDating(s: State, t: TaskDto): Unit = if (Store.canEdit.v) {
  s.dStart := t.startDate.getOrElse(Fmt.today())
  s.dDue := t.dueDate.getOrElse(Fmt.addDays(Fmt.today(), 7))
  s.dating := Some(t)
}

def saveDating(s: State): Unit = s.dating.v.foreach { t =>
  if (s.dStart.v.nonEmpty && s.dDue.v.nonEmpty && s.dDue.v < s.dStart.v) Store.toast("期日は開始日より後にしてください", error = true)
  else Store.report(Api.updateTask(t.id, Json.obj("startDate" -> optS(s.dStart.v), "dueDate" -> optS(s.dDue.v), "version" -> t.version.asJson))) { _ =>
    s.dating := None
    load(s); Store.changed()
  }
}

def optS(v: String): Json = if (v.isEmpty) Json.Null else Json.fromString(v)
def datingTitle(s: State) = s.dating.mapC(_.map(t => s"${t.key} ${t.title}").getOrElse(""))

def editMs(s: State, m: Option[MilestoneDto]): Unit = if (Store.canEdit.v) {
  s.msName := m.map(_.name).getOrElse("")
  s.msDate := m.map(_.date).getOrElse(Fmt.today())
  s.msColor := m.map(_.color).getOrElse("#dc2626")
  s.msEdit := Some(m)
}

def saveMs(s: State): Unit = Store.meta.v.foreach { meta =>
  val r = MilestoneReq(s.msName.v.trim, s.msDate.v, Some(s.msColor.v))
  val f = s.msEdit.v.flatten match {
    case Some(m) => Api.putUnit(s"/api/projects/${meta.project.id}/milestones/${m.id}", r.asJson)
    case None    => Api.post[MilestoneDto](s"/api/projects/${meta.project.id}/milestones", r.asJson).map(_ => ())
  }
  Store.report(f) { _ => s.msEdit := None; load(s); Store.changed() }
}

def removeMs(s: State): Unit = (Store.meta.v, s.msEdit.v.flatten) match {
  case (Some(meta), Some(m)) =>
    Dialog.confirm(s"マイルストーン「${m.name}」を削除しますか？", "削除する") {
      Store.report(Api.delete(s"/api/projects/${meta.project.id}/milestones/${m.id}")) { _ => s.msEdit := None; load(s); Store.changed() }
    }
  case _ => ()
}
def msTitle(s: State) = s.msEdit.mapC(e => if (e.flatten.isDefined) "マイルストーンを編集" else "マイルストーンを追加")
def isEditMs(s: State) = s.msEdit.mapC(_.flatten.isDefined)

def indent(r: Row): String = s"padding-left: ${r.depth * 16 + 6}px"
def rowTitle(r: Row): String = r.t.title
def noDate(s: State, t: TaskDto): Boolean = span(s, t).isEmpty
def collapsedIcon(s: State, r: Row) = s.collapsed.mapC(c => if (!r.hasChildren) "" else if (c.contains(r.t.id)) "▸" else "▾")
def scrollToday(): Unit = Option(dom.document.querySelector(".gantt .right")).foreach { el =>
  Option(dom.document.querySelector(".gantt .today-line")).foreach { t =>
    el.scrollLeft = t.asInstanceOf[dom.HTMLElement].offsetLeft - 200
  }
}
</script>

<template>
  <div class="page wide">
    <div class="row bar-tools">
      <label class="small"><input type="checkbox" v-model="s.done" @change="load(s)"> 完了したタスクも表示</label>
      <span class="grow"></span>
      <label v-if="Store.canEdit" class="small" title="バーの終わりを動かしたとき、依存関係でつながった後続のタスクも同じ日数だけずらす">
        <input type="checkbox" :checked="s.shiftNext" @change="toggleShift(s)"> 後続も一緒にずらす</label>
      <button :class="critBtn(s)" @click="toggleCritical(s)" title="全体の終わりを決めているタスクの並びを強調する">⚡ クリティカルパス</button>
      <button v-if="Store.canEdit" class="small" @click="editMs(s, None)">◆ マイルストーンを追加</button>
      <button class="small" @click="scrollToday()">今日</button>
      <button :class='s.dayW.mapC(w => if (w == 28) "small active-btn" else "small")' @click="s.dayW := 28">日</button>
      <button :class='s.dayW.mapC(w => if (w == 12) "small active-btn" else "small")' @click="s.dayW := 12">週</button>
      <button :class='s.dayW.mapC(w => if (w == 5) "small active-btn" else "small")' @click="s.dayW := 5">月</button>
    </div>
    <p class="muted small">バーをドラッグすると期間を移動、両端をドラッグすると開始日・期日を変えられます。
      バーの右端の丸を別のバーへドラッグすると依存関係（先行 → 後続）をつなぎ、矢印をクリックすると外せます（赤い矢印は順序が守られていない依存関係）。</p>
    <div :class="ganttClass(s)">
      <div class="left">
        <div class="lhead">タスク</div>
        <div v-for="r in rows(s)" :key="r.t.id" class="lrow" :style="indent(r)">
          <span class="tog" @click="toggle(s, r.t.id)">{{ collapsedIcon(s, r) }}</span>
          <a :href='"/tasks/" + r.t.key' target="_blank" rel="noopener" class="key" title="別のタブで開く">{{ r.t.key }}</a>
          <a :href='"/tasks/" + r.t.key' target="_blank" rel="noopener" class="ttl" title="別のタブで開く">{{ rowTitle(r) }}</a>
          <a v-if="noDate(s, r.t)" href="#" class="small nodate" @click.prevent="openDating(s, r.t)" title="開始日・期日を入れる">日付なし</a>
        </div>
      </div>
      <div class="right">
        <div class="timeline" :style="width(s)">
          <div class="months"><div v-for="d in days(s)" class="mcell" :style="dayStyle(s)">{{ monthLabel(d) }}</div></div>
          <div class="msrow">
            <a v-for="m in milestones(s)" :key="m.id" href="#" class="ms-flag" :style="msStyle(s, m)" :title='m.name + "（" + Fmt.day(m.date) + "）ドラッグで移動・クリックで編集"'
              @click.prevent="()" @pointerdown="msDown(s, m, event)">◆ {{ m.name }}</a>
          </div>
          <div class="days"><div v-for="d in days(s)" :class="dayClass(d)" :style="dayStyle(s)">{{ dayLabel(s, d) }}</div></div>
          <div class="grid" :style="height(s)">
            <div class="cols"><div v-for="d in days(s)" :class="dayClass(d)" :style="dayStyle(s)"></div></div>
            <div class="today-line" :style="todayStyle(s)"></div>
            <div v-for="m in milestones(s)" :key="m.id" class="ms-line" :style="msStyle(s, m)"></div>
            <div class="arrows" v-html="arrows(s)" @click="arrowClick(s, event)"></div>
            <div class="arrows link" v-html="linkSvg(s)"></div>
            <div v-for="r in rows(s)" :key="r.t.id" class="grow-row">
              <div :class="barClassC(s, r.t)" :style="barStyle(s, r.t)" :data-bar="r.t.id.toString" @pointerdown='down(s, r.t, "move", event)' :title='r.t.key + " " + r.t.title'>
                <span class="h l" @pointerdown='down(s, r.t, "start", event)'></span>
                <span class="blabel">{{ r.t.title }}</span>
                <span class="h r" @pointerdown='down(s, r.t, "end", event)'></span>
              </div>
              <span v-if="Store.canEdit" class="knob" :style="knobStyle(s, r.t)" title="ドラッグして後続のタスクにつなぐ" @pointerdown="linkStart(s, r.t, event)"></span>
            </div>
          </div>
        </div>
      </div>
    </div>
 <Modal v-if="s.dating.mapC(_.isDefined)" :onClose="_ => s.dating := None">
      <form @submit.prevent="saveDating(s)">
        <h2>日付を入れる</h2>
        <p class="muted small">{{ datingTitle(s) }}</p>
        <div class="row">
          <label class="field grow"><span>開始日</span><input type="date" v-model="s.dStart"></label>
          <label class="field grow"><span>期日</span><input type="date" v-model="s.dDue"></label>
        </div>
        <div class="actions"><button type="button" @click="s.dating := None">キャンセル</button><button type="submit" class="primary">保存</button></div>
      </form>
    </Modal>
    <Modal v-if="s.msEdit.mapC(_.isDefined)" :onClose="_ => s.msEdit := None">
      <form @submit.prevent="saveMs(s)">
        <h2>{{ msTitle(s) }}</h2>
        <p class="muted small">リリース日など、タスクに紐付かない目印をガントチャートに表示します。</p>
        <label class="field"><span>名前</span><input type="text" v-model="s.msName" placeholder="v1.0 リリース" autofocus></label>
        <div class="row">
          <label class="field grow"><span>日付</span><input type="date" v-model="s.msDate"></label>
          <label class="field"><span>色</span><input type="color" v-model="s.msColor"></label>
        </div>
        <div class="actions">
          <button v-if="isEditMs(s)" type="button" class="danger" @click="removeMs(s)">削除</button>
          <span class="grow"></span>
          <button type="button" @click="s.msEdit := None">キャンセル</button><button type="submit" class="primary">保存</button>
        </div>
      </form>
    </Modal>
  </div>
</template>

<style scoped>
.bar-tools { margin-bottom: .4rem; }
.active-btn { background: var(--accent-weak); color: var(--accent); }
.gantt { display: grid; grid-template-columns: 320px 1fr; border: 1px solid var(--line); border-radius: 10px; overflow: hidden; }
@media (max-width: 720px) { .gantt { grid-template-columns: 150px 1fr; } }
.left { border-right: 1px solid var(--line); background: var(--bg); }
.lhead { height: 66px; border-bottom: 1px solid var(--line); display: flex; align-items: flex-end; padding: .3rem .5rem; font-size: .8rem; color: var(--muted); }
.lrow { height: 34px; display: flex; align-items: center; gap: .35rem; border-bottom: 1px solid var(--line); font-size: .84rem; white-space: nowrap; overflow: hidden; }
.tog { width: 1em; cursor: pointer; color: var(--muted); flex: none; }
.ttl { overflow: hidden; text-overflow: ellipsis; color: var(--fg); }
.nodate { margin-left: auto; padding-right: .4rem; flex: none; }
.lrow .key { flex: none; }
.lrow .key:hover { color: var(--accent); text-decoration: underline; }
.right { overflow-x: auto; position: relative; }
.timeline { position: relative; }
.months, .days, .cols { display: flex; }
.months { height: 22px; font-size: .72rem; color: var(--muted); }
.mcell { flex: none; overflow: visible; white-space: nowrap; }
.days { height: 26px; border-bottom: 1px solid var(--line); font-size: .68rem; color: var(--muted); }
.day { flex: none; text-align: center; border-left: 1px solid transparent; }
.cols .day { border-left: 1px solid color-mix(in srgb, var(--line) 60%, transparent); height: 100%; }
.day.weekend { background: color-mix(in srgb, var(--hover) 70%, transparent); }
.days .day.today { color: var(--accent); font-weight: 700; }
.grid { position: relative; }
.cols { position: absolute; inset: 0; }
.today-line { position: absolute; top: 0; bottom: 0; width: 2px; background: var(--danger); opacity: .6; z-index: 1; }
.arrows { position: absolute; inset: 0; pointer-events: none; z-index: 2; }
.grow-row { height: 34px; position: relative; border-bottom: 1px solid color-mix(in srgb, var(--line) 50%, transparent); }
.bar { position: absolute; top: 7px; height: 20px; border-radius: 5px; background: #64748b; color: #fff; font-size: .72rem; display: flex; align-items: center;
  cursor: grab; z-index: 3; overflow: hidden; user-select: none; }
.bar.crit { box-shadow: 0 0 0 2px #f43f5e, 0 0 10px rgba(244, 63, 94, .55); }
.bar.cat-in_progress { background: #2563eb; } .bar.cat-waiting { background: #d97706; } .bar.cat-done { background: #16a34a; opacity: .75; }
.blabel { padding: 0 .4rem; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; flex: 1; }
.h { width: 6px; height: 100%; cursor: ew-resize; flex: none; }
.h:hover { background: rgba(255, 255, 255, .35); }
.bar, .h, .knob { touch-action: none; }
@media (hover: none) { .knob { opacity: .8; width: 14px; height: 14px; top: 10px; } .h { width: 12px; } }
.knob { position: absolute; top: 12px; width: 10px; height: 10px; border-radius: 50%; background: var(--bg); border: 2px solid var(--accent);
  cursor: crosshair; z-index: 4; opacity: 0; transition: opacity .12s; }
.grow-row:hover .knob, .linking .knob { opacity: 1; }
.linking .bar { cursor: copy; }
.linking .bar:hover { outline: 2px solid var(--accent); outline-offset: 1px; }
.arrows.link { z-index: 5; }
.msrow { height: 18px; position: relative; }
.ms-flag { position: absolute; top: 1px; transform: translateX(-6px); font-size: .7rem; white-space: nowrap; color: var(--ms); font-weight: 600;
  cursor: ew-resize; touch-action: none; user-select: none; }
.ms-flag:hover { text-decoration: underline; }
.ms-line { position: absolute; top: 0; bottom: 0; width: 0; border-left: 2px dashed var(--ms); opacity: .8; z-index: 1; }
</style>
