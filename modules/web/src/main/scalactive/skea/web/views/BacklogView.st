<script lang="scala">
import skea.web.*
import io.circe.syntax.*
import org.scalajs.dom
import skea.shared.*
import skea.web.parts.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// バックログ: スプリントごとのタスクと、スプリントに入っていないタスク（ドラッグでスプリント間・順序を移動）
case class State(data: RV[Option[BacklogDto]] = RV(None),
                 editing: RV[Option[Option[SprintDto]]] = RV(None),
                 name: RV[String] = RV(""),
                 goal: RV[String] = RV(""),
                 start: RV[String] = RV(""),
                 end: RV[String] = RV(""),
                 q: RV[String] = RV(""))

def initState() = State()

def pid: Option[UUID] = Store.meta.v.map(_.project.id)

def load(s: State): Unit = pid.foreach(p => Api.backlog(p).foreach(b => s.data := Some(b)))

def mounted(s: State): Unit = {
  pageTitle("バックログ")
  load(s)
  watch(Store.tick, false)((_, _) => load(s))
}

/** 区分（スプリント、None はバックログ）ごとのタスク. */
def tasksOf(s: State, sprint: Option[UUID]) = computed {
  val q = s.q.v.trim.toLowerCase
  s.data.v.toList.flatMap(_.tasks).filter(t => t.sprintId == sprint && (q.isEmpty || t.title.toLowerCase.contains(q) || t.key.toLowerCase.contains(q)))
}
def summary(s: State, sprint: Option[UUID]) = computed {
  val ts = s.data.v.toList.flatMap(_.tasks).filter(_.sprintId == sprint)
  s"${ts.size} 件"
}
def sprints(s: State) = s.data.mapC(_.map(_.sprints).getOrElse(Nil))
def hasActive(s: State) = s.data.mapC(_.exists(_.sprints.exists(_.state == "active")))

// ---------------------------------------------------------------- ドラッグ＆ドロップ

def zone(sprint: Option[UUID]): String = sprint.map(_.toString).getOrElse("backlog")

def rows(s: State, sprint: Option[UUID]) = computed { Dnd.rows(zone(sprint), tasksOf(s, sprint).v) }

def drop(s: State, sprint: Option[UUID], e: dom.Event): Unit = {
  val all = s.data.v.toList.flatMap(_.tasks).filter(_.sprintId == sprint)
  Dnd.drop(zone(sprint), tasksOf(s, sprint).v, e).foreach { (id, after0, before) =>
    val after = after0.orElse(before.flatMap(b => all.takeWhile(_.id != b).filterNot(_.id == id).lastOption.map(_.id)))
    Flip.animate(s.data.v.foreach(b => s.data := Some(b.copy(tasks = Dnd.reinsert(b.tasks, id, after, before)(_.copy(sprintId = sprint))))))
    Api.moveTask(id, MoveTaskReq(None, sprint, Option.when(sprint.isEmpty)(true), after, before)).failed.foreach { err => Toasts.error(err); load(s) }
  }
}

def open(r: DndRow): Unit = if (!Dnd.recentlyDropped) r.task.foreach(t => Router.go("/tasks/" + t.key))

def zoneClass(sprint: Option[UUID]) = Dnd.zoneClass(zone(sprint), "zone")

// ---------------------------------------------------------------- スプリントの操作

def edit(s: State, sp: Option[SprintDto]): Unit = {
  s.name := sp.map(_.name).getOrElse(s"スプリント ${s.data.v.map(_.sprints.size + 1).getOrElse(1)}")
  s.goal := sp.map(_.goal).getOrElse("")
  s.start := sp.flatMap(_.startDate).getOrElse("")
  s.end := sp.flatMap(_.endDate).getOrElse("")
  s.editing := Some(sp)
}

def save(s: State): Unit = pid.foreach { p =>
  val r = SprintReq(s.name.v.trim, Some(s.goal.v), Option(s.start.v).filter(_.nonEmpty), Option(s.end.v).filter(_.nonEmpty))
  val f = s.editing.v.flatten match {
    case Some(sp) => Api.putUnit(s"/api/projects/$p/sprints/${sp.id}", r.asJson)
    case None     => Api.post[SprintDto](s"/api/projects/$p/sprints", r.asJson).map(_ => ())
  }
  Toasts.report(f) { _ => s.editing := None; Store.reloadMeta(); load(s) }
}

def start(s: State, sp: SprintDto): Unit = pid.foreach { p =>
  Toasts.report(Api.post[SprintDto](s"/api/projects/$p/sprints/${sp.id}/start")) { _ =>
    Toasts.show(s"「${sp.name}」を開始しました", Store.meta.v.map(m => s"/p/${m.project.key}/board"))
    Store.reloadMeta(); load(s)
  }
}

def remove(s: State, sp: SprintDto): Unit = pid.foreach { p =>
  Dialog.confirm(s"「${sp.name}」を削除しますか？（タスクはバックログに戻ります）") {
    Toasts.report(Api.delete(s"/api/projects/$p/sprints/${sp.id}")) { _ => Store.reloadMeta(); load(s) }
  }
}

def create(): Unit = pid.foreach(p => Store.createDialog := Some((p, None)))
def stateLabel(sp: SprintDto): String = if (sp.state == "active") "進行中" else "計画中"
def dates(sp: SprintDto): String = (sp.startDate, sp.endDate) match {
  case (None, None) => ""
  case (a, b)       => s"${a.map(Fmt.day).getOrElse("")} 〜 ${b.map(Fmt.day).getOrElse("")}"
}
def editTitle(s: State) = s.editing.mapC(e => if (e.flatten.isDefined) "スプリントを編集" else "スプリントを作成")
</script>

<template>
  <div class="page">
    <div class="row head">
      <input type="search" v-model="s.q" placeholder="絞り込み">
      <span class="grow"></span>
      <button v-if="Store.canEdit" @click="edit(s, None)">＋ スプリントを作成</button>
    </div>
    <section v-for="sp in sprints(s)" :key="sp.id" :class="zoneClass(Some(sp.id))" @dragover="Dnd.over(zone(Some(sp.id)), event)" @drop="drop(s, Some(sp.id), event)">
      <header class="row">
        <b>{{ sp.name }}</b>
        <span :class='if (sp.state == "active") "badge cat-in_progress" else "badge"'>{{ stateLabel(sp) }}</span>
        <span class="muted small">{{ dates(sp) }}</span>
        <span class="muted small">{{ summary(s, Some(sp.id)) }}</span>
        <span class="grow"></span>
        <button v-if='Store.canEdit.v && sp.state == "planned"' class="small" :disabled="hasActive(s)" @click="start(s, sp)">開始</button>
        <button v-if="Store.canEdit" class="ghost small" @click="edit(s, Some(sp))">編集</button>
        <button v-if='Store.canEdit.v && sp.state != "active"' class="ghost small" @click="remove(s, sp)">削除</button>
      </header>
      <div v-if="sp.goal.nonEmpty" class="muted small goal">🎯 {{ sp.goal }}</div>
      <div class="rows">
        <div v-for="r in rows(s, Some(sp.id))" :key="r.key" :class="Dnd.cls(r)" :data-task="r.id" :style="Dnd.gapStyle(r)"
          :draggable='if (r.task.isDefined) "true" else "false"' @dragstart="r.task.foreach(t => Dnd.start(t, zone(Some(sp.id)), tasksOf(s, Some(sp.id)).v, event))"
          @dragend="Dnd.end()" @click="open(r)">
          <TaskRow v-if="r.task.isDefined" :task="r.task.get" />
        </div>
        <div v-if="tasksOf(s, Some(sp.id)).mapC(_.isEmpty)" class="muted small hint">タスクをここへドラッグして計画します</div>
      </div>
    </section>
    <section :class="zoneClass(None)" @dragover="Dnd.over(zone(None), event)" @drop="drop(s, None, event)">
      <header class="row"><b>バックログ</b><span class="muted small">{{ summary(s, None) }}</span><span class="grow"></span>
        <button v-if="Store.canEdit" class="small" @click="create()">＋ タスク</button></header>
      <div class="rows">
        <div v-for="r in rows(s, None)" :key="r.key" :class="Dnd.cls(r)" :data-task="r.id" :style="Dnd.gapStyle(r)"
          :draggable='if (r.task.isDefined) "true" else "false"' @dragstart="r.task.foreach(t => Dnd.start(t, zone(None), tasksOf(s, None).v, event))"
          @dragend="Dnd.end()" @click="open(r)">
          <TaskRow v-if="r.task.isDefined" :task="r.task.get" />
        </div>
        <div v-if="tasksOf(s, None).mapC(_.isEmpty)" class="muted small hint">バックログは空です</div>
      </div>
    </section>
    <Modal v-if="s.editing.mapC(_.isDefined)" :onClose="_ => s.editing := None">
      <form @submit.prevent="save(s)">
          <h2>{{ editTitle(s) }}</h2>
          <label class="field"><span>名前</span><input type="text" v-model="s.name"></label>
          <label class="field"><span>ゴール</span><input type="text" v-model="s.goal"></label>
          <div class="row">
            <label class="field grow"><span>開始日</span><input type="date" v-model="s.start"></label>
            <label class="field grow"><span>終了日</span><input type="date" v-model="s.end"></label>
          </div>
          <div class="actions"><button type="button" @click="s.editing := None">キャンセル</button><button type="submit" class="primary">保存</button></div>
        </form>
    </Modal>
  </div>
</template>

<style scoped>
.head { margin-bottom: 1rem; }
.zone { border: 2px solid transparent; background: var(--col); border-radius: 10px; padding: .6rem; margin-bottom: 1rem; }
.zone.over { border-color: var(--accent); }
.zone header { margin-bottom: .4rem; }
.goal { margin: -.2rem 0 .4rem; }
.rows { display: flex; flex-direction: column; gap: 2px; }
.hint { padding: .6rem; text-align: center; }
</style>
