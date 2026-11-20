<script lang="scala">
import skea.web.*
import io.circe.syntax.*
import skea.shared.*

import java.util.UUID
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// 作業ログの編集（作業時間の一覧のセル・明細から開く）. 自分のログ（管理者はすべて）を直せる.
// addTo を渡すと、そのタスク・日付に自分の作業時間を追加できる.
case class Props(logs: List[WorklogDto], title: String, addTo: Option[(UUID, String)] = None, onClose: Unit => Unit, onSaved: Unit => Unit)

/** 編集中の行（作業日・時間・内容）. */
case class Row(w: WorklogDto, date: RV[String], time: RV[String], note: RV[String])

case class State(rows: List[Row], act: ActionState, newTime: RV[String] = RV(""), newNote: RV[String] = RV(""))

def initState(props: Props) = State(props.logs.sortBy(_.workDate).map(w => Row(w, RV(w.workDate), RV(Minutes.format(w.minutes)), RV(w.note))), useAction())

def editable(w: WorklogDto): Boolean = Store.myId.contains(w.user.id) || Store.isAdmin.v

def done(props: Props, msg: String): Unit = { Toasts.show(msg); Store.changed(); props.onSaved(()); props.onClose(()) }

/** 変えた行だけを保存する. */
def save(s: State, props: Props): Unit = {
  val changed = s.rows.filter(r => editable(r.w) && (r.date.v != r.w.workDate || Minutes.parse(r.time.v).exists(_ != r.w.minutes) || r.note.v != r.w.note))
  val bad = changed.find(r => Minutes.parse(r.time.v).forall(m => m <= 0 || m > 24 * 60))
  val add = Minutes.parse(s.newTime.v).filter(_ => props.addTo.isDefined)
  if (bad.isDefined || (s.newTime.v.trim.nonEmpty && add.isEmpty)) Toasts.show("時間は 1h30m、90m、1.5h のように入力してください（24時間まで）", error = true)
  else if (changed.isEmpty && add.isEmpty) props.onClose(())
  else {
    val updates = changed.map(r => Api.putUnit(s"/api/worklogs/${r.w.id}", WorklogReq(r.date.v, Minutes.parse(r.time.v).get, Some(r.note.v.trim)).asJson))
    val created = add.toList.flatMap(m => props.addTo.map((tid, d) => Api.addWorklog(tid, WorklogReq(d, m, Some(s.newNote.v.trim))).map(_ => ())))
    s.act.run(Future.sequence(updates ++ created)).onComplete { r =>
      r.failed.foreach(Toasts.error)
      if (r.isSuccess) done(props, "作業時間を保存しました")
    }
  }
}

def remove(s: State, props: Props, r: Row): Unit =
  Dialog.confirm(s"${Fmt.day(r.w.workDate)} の作業時間（${Minutes.format(r.w.minutes)}）を削除しますか？", "削除する") {
    Toasts.report(Api.delete(s"/api/worklogs/${r.w.id}"))(_ => done(props, "作業時間を削除しました"))
  }

def total(s: State) = Minutes.format(s.rows.map(_.w.minutes).sum)
</script>

<template>
  <Modal :onClose="_ => props.onClose(())">
    <form @submit.prevent="save(s, props)">
      <h2>作業時間の編集</h2>
      <p class="muted small">{{ props.title }}</p>
      <div v-for="r in s.rows" :key="r.w.id" class="wl">
        <div class="who small"><Avatar :user="Some(person(r.w.user))" :size="18" /> {{ r.w.user.name }}
          <RouterLink :to='"/tasks/" + r.w.taskKey' class="key">{{ r.w.taskKey }}</RouterLink> <span class="muted ell">{{ r.w.taskTitle }}</span></div>
        <div class="row fields">
          <input type="date" v-model="r.date" :disabled="!editable(r.w)">
          <input type="text" v-model="r.time" class="time" :disabled="!editable(r.w)" placeholder="1h30m">
          <input type="text" v-model="r.note" class="grow" :disabled="!editable(r.w)" placeholder="作業の内容（任意）">
          <button v-if="editable(r.w)" type="button" class="ghost small" @click="remove(s, props, r)" title="削除">🗑</button>
        </div>
      </div>
      <p v-if="s.rows.isEmpty" class="muted small">この日の作業時間はまだありません。</p>
      <div v-if="props.addTo.isDefined" class="wl add">
        <div class="small muted">自分の作業時間を追加</div>
        <div class="row fields">
          <input type="text" v-model="s.newTime" class="time" placeholder="1h30m">
          <input type="text" v-model="s.newNote" class="grow" placeholder="作業の内容（任意）">
        </div>
      </div>
      <div class="actions">
        <span class="muted small grow">合計 {{ total(s) }}</span>
        <button type="button" @click="props.onClose(())">キャンセル</button>
        <button type="submit" class="primary" :disabled="s.act.busy">保存</button>
      </div>
    </form>
  </Modal>
</template>

<style scoped>
.wl { border-top: 1px solid var(--line); padding: .6rem 0; }
.who { display: flex; align-items: center; gap: .4rem; margin-bottom: .35rem; min-width: 0; }
.ell { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.fields { flex-wrap: nowrap; }
.fields input[type="date"] { width: 9.5rem; }
.time { width: 6rem; }
.add { background: var(--side); border-radius: 8px; padding: .6rem; border-top: none; }
.actions { align-items: center; }
</style>
