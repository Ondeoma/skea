<script lang="scala">
import skea.web.*
import io.circe.syntax.*
import org.scalajs.dom
import skea.shared.*
import skea.web.parts.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// カンバン: アクティブなスプリントのタスクをステータスの列に並べる（ドラッグで移動. クリックで詳細をモーダルで出す）
case class State(board: RV[Option[BoardDto]] = RV(None),
                 q: RV[String] = RV(""),
                 who: RV[String] = RV(""),
                 label: RV[String] = RV(""),
                 opened: RV[Option[String]] = RV(None),
                 laneBy: RV[String] = RV(Prefs.get("board.lanes", "")),
                 folded: RV[Set[String]] = RV(Set.empty),
                 completing: RV[Boolean] = RV(false),
                 moveTo: RV[String] = RV(""))

def initState() = State()

def load(s: State): Unit = Store.meta.v.foreach(m => Api.board(m.project.id).foreach(b => s.board := Some(b)))

def mounted(s: State): Unit = {
  pageTitle("ボード")
  load(s)
  watch(Store.tick, false)((_, _) => load(s))
}

def statuses = Store.meta.mapC(_.map(_.statuses).getOrElse(Nil))
def members = Store.meta.mapC(_.map(_.members.map(_.user)).getOrElse(Nil))
def labels = Store.meta.mapC(_.map(_.labels).getOrElse(Nil))

private def visible(s: State, t: TaskDto): Boolean = {
  val q = s.q.v.trim.toLowerCase
  (q.isEmpty || t.title.toLowerCase.contains(q) || t.key.toLowerCase.contains(q)) &&
  (s.who.v match {
    case ""     => true
    case "none" => t.assignee.isEmpty
    case "me"   => t.assignee.exists(a => Store.myId.contains(a.id))
    case id     => t.assignee.exists(_.id.toString == id)
  }) &&
  (s.label.v.isEmpty || t.labelIds.exists(_.toString == s.label.v))
}

// ---------------------------------------------------------------- スイムレーン（担当者・ラベルごとの横の区切り）

/** レーン. key は "" （区切りなし）、"u:<ユーザー>"・"u:none"、"l:<ラベル>"・"l:none". */
final case class Lane(key: String, title: String, user: Option[UserBrief], color: Option[String])

def lanes(s: State) = computed {
  val ts = s.board.v.toList.flatMap(_.tasks).filter(visible(s, _))
  s.laneBy.v match {
    case "assignee" =>
      ts.flatMap(_.assignee).distinctBy(_.id).sortBy(_.name).map(u => Lane(s"u:${u.id}", u.name, Some(u), None)) ++
        (if (ts.exists(_.assignee.isEmpty)) List(Lane("u:none", "未割り当て", None, None)) else Nil)
    case "label" =>
      Store.meta.v.toList.flatMap(_.labels).filter(l => ts.exists(_.labelIds.contains(l.id))).map(l => Lane(s"l:${l.id}", l.name, None, Some(l.color))) ++
        (if (ts.exists(_.labelIds.isEmpty)) List(Lane("l:none", "ラベルなし", None, None)) else Nil)
    case _ => List(Lane("", "", None, None))
  }
}

def inLane(ln: Lane, t: TaskDto): Boolean = ln.key match {
  case ""                       => true
  case "u:none"                 => t.assignee.isEmpty
  case k if k.startsWith("u:")  => t.assignee.exists(_.id.toString == k.drop(2))
  case "l:none"                 => t.labelIds.isEmpty
  case k                        => t.labelIds.exists(_.toString == k.drop(2))
}

val laneOpts: List[Opt] = Opt.of(List("" -> "区切りなし", "assignee" -> "担当者ごとに区切る", "label" -> "ラベルごとに区切る"))
def setLanes(s: State, v: String): Unit = { s.laneBy := v; s.folded := Set.empty; Prefs.set("board.lanes", v) }
def fold(s: State, ln: Lane): Unit = s.folded := (if (s.folded.v(ln.key)) s.folded.v - ln.key else s.folded.v + ln.key)
def isOpen(s: State, ln: Lane) = s.folded.mapC(!_.contains(ln.key))
def foldIcon(s: State, ln: Lane) = s.folded.mapC(f => if (f.contains(ln.key)) "▸" else "▾")
def laneCount(s: State, ln: Lane) = computed { s.board.v.toList.flatMap(_.tasks).count(t => visible(s, t) && inLane(ln, t)).toString }
def laneMode(s: State) = s.laneBy.mapC(_.nonEmpty)
def colsClass(s: State) = s.laneBy.mapC(l => if (l.isEmpty) "cols" else "cols laned")
def zone(st: StatusDto, ln: Lane): String = st.id.toString + "|" + ln.key

/** 列（とレーン）のタスク（絞り込みを反映）. 絞り込みの値も監視するため computed にする. */
def column(s: State, st: StatusDto, ln: Lane) = computed {
  s.board.v.toList.flatMap(_.tasks).filter(t => t.statusId == st.id && visible(s, t) && inLane(ln, t))
}
def rows(s: State, st: StatusDto, ln: Lane) = computed { Dnd.rows(zone(st, ln), column(s, st, ln).v) }
def count(s: State, st: StatusDto) = computed { s.board.v.toList.flatMap(_.tasks).count(t => t.statusId == st.id && visible(s, t)).toString }

def sprint(s: State) = s.board.mapC(_.flatMap(_.sprint))
def hasSprint(s: State) = s.board.mapC(_.exists(_.sprint.isDefined))
def noSprint(s: State) = s.board.mapC(b => b.isDefined && b.exists(_.sprint.isEmpty))
def sprintInfo(s: State) = s.board.mapC(_.flatMap(_.sprint).map { sp =>
  val rest = sp.endDate.map { e =>
    val days = ((new scala.scalajs.js.Date(e + "T23:59:59")).getTime() - scala.scalajs.js.Date.now()) / 86400000
    if (days < 0) "（期間を過ぎています）" else s"（残り ${days.toInt} 日）"
  }.getOrElse("")
  s"${sp.startDate.map(Fmt.day).getOrElse("")} 〜 ${sp.endDate.map(Fmt.day).getOrElse("")} $rest"
}.getOrElse(""))
def goal(s: State) = s.board.mapC(_.flatMap(_.sprint).map(_.goal).getOrElse(""))
def plannedSprints = Store.meta.mapC(_.map(_.sprints.filter(_.state == "planned")).getOrElse(Nil))

// ---------------------------------------------------------------- ドラッグ＆ドロップ

def drop(s: State, st: StatusDto, ln: Lane, e: dom.Event): Unit = {
  // 並び順は列の全タスク（絞り込みで隠れているものも含む）の中で決める
  val all = s.board.v.toList.flatMap(_.tasks).filter(_.statusId == st.id)
  Dnd.drop(zone(st, ln), column(s, st, ln).v, e).foreach { (id, after0, before) =>
    val after = after0.orElse(before.flatMap(b => all.takeWhile(_.id != b).filterNot(_.id == id).lastOption.map(_.id)))
    // 担当者のレーンをまたいだら担当者も変える
    val assignee: Option[Option[UserBrief]] = ln.key match {
      case "u:none"                => Some(None)
      case k if k.startsWith("u:") => lanes(s).v.find(_.key == k).map(_.user)
      case _                       => None
    }
    val before0 = s.board.v.toList.flatMap(_.tasks).find(_.id == id)
    Flip.animate(s.board.v.foreach(b => s.board := Some(b.copy(tasks = Dnd.reinsert(b.tasks, id, after, before)(t =>
      assignee.fold(t)(a => t.copy(assignee = a)).copy(statusId = st.id))))))
    Api.moveTask(id, MoveTaskReq(Some(st.id), None, None, after, before)).flatMap { _ =>
      assignee.filter(a => before0.exists(_.assignee.map(_.id) != a.map(_.id))) match {
        case Some(a) => Api.updateTask(id, io.circe.Json.obj("assigneeId" -> a.map(_.id).asJson)).map(_ => Store.changed())
        case None    => scala.concurrent.Future.successful(())
      }
    }.failed.foreach { err => Store.error(err); load(s) }
  }
}

/** カードのクリック: 詳細をモーダルで出す（ドラッグの直後は無視）. */
def open(s: State, r: DndRow): Unit = if (!Dnd.recentlyDropped) r.task.foreach(t => s.opened := Some(t.key))

def closeTask(s: State): Unit = { s.opened := None; load(s) }

def colClass(st: StatusDto, ln: Lane) = Dnd.zoneClass(zone(st, ln), "col")
def whoClass(s: State, w: String) = s.who.mapC(x => if (x == w) "chip active" else "chip")

def complete(s: State): Unit = sprint(s).v.foreach { sp =>
  Store.meta.v.foreach { m =>
    val to = Option(s.moveTo.v).filter(_.nonEmpty).map(UUID.fromString)
    Store.report(Api.post[io.circe.Json](s"/api/projects/${m.project.id}/sprints/${sp.id}/complete", CompleteSprintReq(to).asJson)) { r =>
      s.completing := false
      Store.toast(s"スプリント「${sp.name}」を完了しました（未完了 ${r.hcursor.get[Int]("moved").getOrElse(0)} 件を移動）")
      Store.reloadMeta()
      Store.changed()
      load(s)
    }
  }
}
def backlogLink: String = Store.meta.v.map(m => "/p/" + m.project.key + "/backlog").getOrElse("/")
</script>

<template>
  <div class="page wide board-page">
    <div v-if="hasSprint(s)" class="sprint-bar">
      <div>
        <b>{{ sprint(s).mapC(_.map(_.name).getOrElse("")) }}</b>
        <span class="muted small">{{ sprintInfo(s) }}</span>
        <div v-if="goal(s).mapC(_.nonEmpty)" class="small muted">🎯 {{ goal(s) }}</div>
      </div>
      <span class="grow"></span>
      <button v-if="Store.canEdit" class="small" @click="s.completing := true">スプリントを完了</button>
    </div>
    <div class="filters">
      <input type="search" v-model="s.q" placeholder="絞り込み" class="fq">
      <div class="avatars">
        <button :class='whoClass(s, "")' @click='s.who := ""'>全員</button>
        <button :class='whoClass(s, "me")' @click='s.who := (if (s.who.v == "me") "" else "me")' title="自分が担当のタスクだけ">👤 自分</button>
        <button v-for="u in members" :key="u.id" :class='s.who.mapC(w => if (w == u.id.toString) "av active" else "av")' @click="s.who := (if (s.who.v == u.id.toString) &quot;&quot; else u.id.toString)">
          <Avatar :user="Some(u)" :size="26" />
        </button>
        <button :class='whoClass(s, "none")' @click='s.who := "none"'>未割り当て</button>
      </div>
      <Combo :bind="Some(s.label)" :live="Some(labels.mapC(_.map(l => Opt(l.id.toString, l.name, color = l.color))))" clearLabel="すべてのラベル" :small="true" />
      <Combo :options="laneOpts" :value="s.laneBy.v" :small="true" :onChange="v => setLanes(s, v)" />
    </div>
    <div v-if="noSprint(s)" class="empty card">
      <p>進行中のスプリントはありません。</p>
      <p><RouterLink :to="backlogLink">バックログ</RouterLink>でスプリントを作成・開始すると、ここにタスクが並びます。</p>
    </div>
    <div v-if="hasSprint(s)" class="board">
      <div :class="colsClass(s)">
        <header v-for="st in statuses" :key="st.id" class="col-head"><span class="dot" :style='"background:" + st.color'></span>{{ st.name }} <span class="muted">{{ count(s, st) }}</span></header>
      </div>
      <div v-for="ln in lanes(s)" :key="ln.key" class="lane">
        <div v-if="laneMode(s)" class="lane-head" @click="fold(s, ln)">
          <span class="muted">{{ foldIcon(s, ln) }}</span>
          <Avatar v-if='ln.key.startsWith("u:")' :user="ln.user" :size="20" />
          <span v-if="ln.color.isDefined" class="label-chip" :style='"background:" + ln.color.getOrElse("")'>{{ ln.title }}</span>
          <b v-else>{{ ln.title }}</b>
          <span class="muted small">{{ laneCount(s, ln) }}</span>
        </div>
        <div v-if="isOpen(s, ln)" :class="colsClass(s)">
          <section v-for="st in statuses" :key="st.id" :class="colClass(st, ln)" @dragover="Dnd.over(zone(st, ln), event)" @drop="drop(s, st, ln, event)">
            <div class="cards">
              <div v-for="r in rows(s, st, ln)" :key="r.key" :class="Dnd.cls(r)" :data-task="r.id" :style="Dnd.gapStyle(r)"
                :draggable='if (r.task.isDefined) "true" else "false"' @dragstart="r.task.foreach(t => Dnd.start(t, zone(st, ln), column(s, st, ln).v, event))"
                @dragend="Dnd.end()" @click="open(s, r)">
                <TaskCard v-if="r.task.isDefined" :task="r.task.get" :meta="Store.meta.v" />
              </div>
            </div>
          </section>
        </div>
      </div>
    </div>
 <Modal v-for="k in s.opened.mapC(_.toList)" :key="k" :xwide="true" :onClose="_ => closeTask(s)">
      <TaskView :taskKey="k" :modal="true" :onClose="_ => closeTask(s)" />
    </Modal>
    <Modal v-if="s.completing" :onClose="_ => s.completing := false">
      <h2>スプリントを完了</h2>
          <p>完了していないタスクの移動先を選んでください。</p>
          <Combo :bind="Some(s.moveTo)" :live="Some(plannedSprints.mapC(_.map(sp => Opt(sp.id.toString, sp.name))))" clearLabel="バックログ" />
          <div class="actions">
            <button @click="s.completing := false">キャンセル</button>
            <button class="primary" @click="complete(s)">完了する</button>
          </div>
    </Modal>
  </div>
</template>

<style scoped>
.board-page { padding-bottom: 1rem; }
.sprint-bar { display: flex; align-items: center; gap: .6rem; margin-bottom: .7rem; }
.filters { display: flex; gap: .6rem; align-items: center; flex-wrap: wrap; margin-bottom: .8rem; }
.fq { width: 200px; }
.avatars { display: flex; gap: .25rem; align-items: center; flex-wrap: wrap; }
.av { border: 2px solid transparent; border-radius: 50%; padding: 0; background: none; }
.av.active { border-color: var(--accent); }
.board { overflow-x: auto; padding-bottom: .5rem; }
.cols { display: grid; grid-auto-flow: column; grid-auto-columns: minmax(250px, 1fr); gap: .8rem; align-items: stretch; min-width: max-content; }
.col-head { font-weight: 600; font-size: .85rem; padding: .3rem .5rem .4rem; display: flex; align-items: center; gap: .4rem; }
.col { background: var(--col); border-radius: 10px; padding: .5rem; min-height: 60vh; border: 2px solid transparent; }
.cols.laned .col { min-height: 70px; }
.col.over { border-color: var(--accent); }
.lane { margin-bottom: .6rem; }
.lane-head { display: flex; align-items: center; gap: .45rem; padding: .35rem .3rem; cursor: pointer; position: sticky; left: 0; width: max-content; }
.dot { width: 10px; height: 10px; border-radius: 50%; display: inline-block; }
.cards { display: flex; flex-direction: column; gap: .45rem; }
@media (max-width: 720px) { .cols { grid-auto-columns: 82vw; } .fq { width: 100%; } }
</style>
