<script lang="scala">
import skea.web.*
import skea.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// タスクの作業時間（記録・一覧）と変更履歴
case class Props(taskId: UUID, canEdit: Boolean, tab: String, onChanged: Unit => Unit = _ => ())

case class State(logs: RV[List[WorklogDto]] = RV(Nil),
                 history: RV[List[EventDto]] = RV(Nil),
                 date: RV[String] = RV(Fmt.today()),
                 time: RV[String] = RV(""),
                 note: RV[String] = RV(""),
                 error: RV[String] = RV(""))

def initState() = State()

def load(s: State, props: Props): Unit =
  if (props.tab == "history") Api.history(props.taskId).foreach(s.history := _)
  else Api.worklogs(props.taskId).foreach(s.logs := _)

def mounted(s: State, props: Props): Unit = {
  load(s, props)
  watch(Store.tick, false)((_, _) => load(s, props))
}

def add(s: State, props: Props): Unit = Minutes.parse(s.time.v) match {
  case None => s.error := "時間は 1h30m、90m、1.5h のように入力してください"
  case Some(m) =>
    s.error := ""
    Toasts.report(Api.addWorklog(props.taskId, WorklogReq(s.date.v, m, Some(s.note.v)))) { _ =>
      s.time := ""; s.note := ""
      load(s, props)
      props.onChanged(())
      Store.changed()
    }
}

def remove(s: State, props: Props, w: WorklogDto): Unit =
  Dialog.confirm("この作業時間を削除しますか？") {
    Toasts.report(Api.delete(s"/api/worklogs/${w.id}")) { _ => load(s, props); props.onChanged(()) }
  }

def total(s: State) = s.logs.mapC(ls => Minutes.format(ls.map(_.minutes).sum))
def mine(w: WorklogDto): Boolean = Store.myId.contains(w.user.id)

def changes(e: EventDto): List[String] =
  e.detail.hcursor.downField("changes").as[List[io.circe.Json]].getOrElse(Nil).flatMap { c =>
    val f = c.hcursor.get[String]("field").getOrElse("")
    val from = c.hcursor.downField("from").focus.filterNot(_.isNull).map(j => j.asString.getOrElse(j.noSpaces))
    val to = c.hcursor.downField("to").focus.filterNot(_.isNull).map(j => j.asString.getOrElse(j.noSpaces))
    if (Set("title", "startDate", "dueDate")(f)) List(s"${TaskPatch.label(f)}: ${from.getOrElse("なし")} → ${to.getOrElse("なし")}") else Nil
  }
def isHistory(props: Props): Boolean = props.tab == "history"
</script>

<template>
  <div v-if="isHistory(props)">
    <div v-for="e in s.history" :key="e.id" class="ev">
      <Avatar :user="e.actor.map(person)" :size="22" />
      <div>
        <b>{{ Store.actorName(e) }}</b> さんが {{ EventText.describe(e.kind, e.detail, None) }}
        <div v-for="c in changes(e)" class="muted small">{{ c }}</div>
        <div class="muted small">{{ Fmt.dateTime(e.at) }} {{ if (e.via == "web") "" else "（" + e.via + "）" }}</div>
      </div>
    </div>
  </div>
  <div v-else>
    <form v-if="props.canEdit" class="row add" @submit.prevent="add(s, props)">
      <input type="date" v-model="s.date">
      <input type="text" v-model="s.time" placeholder="1h30m" class="time">
      <input type="text" v-model="s.note" placeholder="作業内容（任意）" class="grow">
      <button type="submit" class="primary small">記録</button>
    </form>
    <p v-if="s.error.mapC(_.nonEmpty)" class="error small">{{ s.error }}</p>
    <table class="list">
      <tbody>
        <tr v-for="w in s.logs" :key="w.id">
          <td class="nowrap">{{ Fmt.day(w.workDate) }}</td>
          <td class="nowrap"><Avatar :user="Some(person(w.user))" :size="20" /> {{ w.user.name }}</td>
          <td class="nowrap"><b>{{ Minutes.format(w.minutes) }}</b></td>
          <td class="muted">{{ w.note }}</td>
          <td><button v-if="mine(w) && props.canEdit" class="ghost small" @click="remove(s, props, w)">削除</button></td>
        </tr>
      </tbody>
    </table>
    <p class="small muted">合計 {{ total(s) }}</p>
  </div>
</template>

<style scoped>
.ev { display: flex; gap: .6rem; padding: .45rem 0; border-bottom: 1px solid var(--line); font-size: .88rem; }
.add { margin-bottom: .6rem; }
.time { width: 6rem; }
</style>
