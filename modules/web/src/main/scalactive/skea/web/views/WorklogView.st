<script lang="scala">
import skea.web.*
import skea.shared.*
import skea.web.parts.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

// 作業時間の一覧: 誰が日ごとにどの作業をしていたか（人×日の表、メンバーを選んだタスク×日の表、日ごとの明細）
case class State(from: RV[String] = RV(""),
                 to: RV[String] = RV(""),
                 project: RV[String] = RV(""),
                 user: RV[String] = RV(""),
                 list: RV[List[WorklogDto]] = RV(Nil),
                 view: RV[String] = RV("table"),
                 users: RV[List[UserBrief]] = RV(Nil),
                 editing: RV[Option[Editing]] = RV(None))

/** 編集のモーダル（対象のログ・見出し・追加できるタスクと日付）. */
final case class Editing(logs: List[WorklogDto], title: String, addTo: Option[(UUID, String)])

/** 今週の月曜から日曜. */
def initState() = {
  val d = new js.Date()
  val dow = (d.getDay().toInt + 6) % 7
  val mon = Fmt.addDays(Fmt.isoDate(d), -dow)
  State(from = RV(mon), to = RV(Fmt.addDays(mon, 6)))
}

def load(s: State): Unit =
  Store.report(Api.worklogReport(s.from.v, s.to.v, Option(s.project.v).filter(_.nonEmpty).map(UUID.fromString),
    Option(s.user.v).filter(_.nonEmpty).map(UUID.fromString)))(s.list := _)

def mounted(s: State): Unit = {
  pageTitle("作業時間の一覧")
  if (Store.projects.v.isEmpty) Store.loadProjects()
  Api.users("").foreach(us => s.users := us.sortBy(_.name))
  load(s)
}

/** 表示の切り替え. メンバー別はメンバーを1人選ぶ（未選択なら自分）. */
def setView(s: State, v: String): Unit = {
  s.view := v
  if (v == "person" && s.user.v.isEmpty) { s.user := Store.myId.map(_.toString).getOrElse(""); load(s) }
}

def thisMonth(s: State): Unit = {
  val d = new js.Date()
  val first = f"${d.getFullYear().toInt}%04d-${d.getMonth().toInt + 1}%02d-01"
  val next = new js.Date(d.getFullYear().toInt, d.getMonth().toInt + 1, 1)
  s.from := first
  s.to := Fmt.addDays(Fmt.isoDate(next), -1)
  load(s)
}

/** 選んだメンバーのタスクごとの行と、日ごとの合計. */
def personTasks(s: State) = computed { Option(s.user.v).filter(_.nonEmpty).map(UUID.fromString).toList.flatMap(u => tasksOf(s, u).v) }
def personDays(s: State) = computed { days(s).v.map(d => s.list.v.filter(w => w.workDate == d && w.user.id.toString == s.user.v).map(_.minutes).sum) }
def personTotal(s: State) = computed { Minutes.hours(s.list.v.filter(_.user.id.toString == s.user.v).map(_.minutes).sum) }
/** セルの説明（その日の作業内容）. */
def notes(s: State, w: WorklogDto, day: String): String =
  s.list.v.filter(x => x.taskId == w.taskId && x.user.id == w.user.id && x.workDate == day).map(x => Minutes.format(x.minutes) + Option(x.note).filter(_.nonEmpty).fold("")(n => " " + n)).mkString("\n")
def dayIdx(s: State) = days(s).mapC(_.zipWithIndex)

/** セル（タスク × 人 × 日）をクリック: その日のそのタスクのログを編集する（自分の行なら追加もできる）. */
def openCell(s: State, w: WorklogDto, day: String): Unit = {
  val logs = s.list.v.filter(x => x.taskId == w.taskId && x.user.id == w.user.id && x.workDate == day)
  val mine = Store.myId.contains(w.user.id)
  if (logs.nonEmpty || mine)
    s.editing := Some(Editing(logs, s"${w.taskKey} ${w.taskTitle} · ${w.user.name} · ${Fmt.day(day)}", Option.when(mine)((w.taskId, day))))
}

def openLog(s: State, w: WorklogDto): Unit = s.editing := Some(Editing(List(w), s"${w.taskKey} ${w.taskTitle} · ${Fmt.day(w.workDate)}", None))

def cellClass(m: Int, day: String): String = weekend(day) + " cellbtn" + (if (m > 0) " has" else "")

def shift(s: State, n: Int): Unit = {
  s.from := Fmt.addDays(s.from.v, n)
  s.to := Fmt.addDays(s.to.v, n)
  load(s)
}

def days(s: State) = computed {
  val f = s.from.v
  val n = math.max(0, math.min(62, ((new js.Date(s.to.v).getTime() - new js.Date(f).getTime()) / 86400000).toInt))
  (0 to n).toList.map(i => Fmt.addDays(f, i))
}

/** 人ごとの行（人, 日ごとの分, 合計）. */
def people(s: State) = computed {
  s.list.v.groupBy(_.user.id).toList.map { (_, ws) =>
    val u = ws.head.user
    (u, days(s).v.map(d => ws.filter(_.workDate == d).map(_.minutes).sum), ws.map(_.minutes).sum)
  }.sortBy(_._1.name)
}

/** 人・タスクごとの行（日ごとの分）. */
def tasksOf(s: State, uid: UUID) = computed {
  s.list.v.filter(_.user.id == uid).groupBy(_.taskId).toList.map { (_, ws) =>
    (ws.head, days(s).v.map(d => ws.filter(_.workDate == d).map(_.minutes).sum), ws.map(_.minutes).sum)
  }.sortBy(_._1.taskKey)
}

def dayTotals(s: State) = computed { days(s).v.map(d => s.list.v.filter(_.workDate == d).map(_.minutes).sum) }
def total(s: State) = s.list.mapC(ls => Minutes.format(ls.map(_.minutes).sum))
def byDay(s: State) = computed { s.list.v.groupBy(_.workDate).toList.sortBy(_._1).reverse }
def cell(m: Int): String = if (m == 0) "" else Minutes.hours(m)
def dayHead(d: String): String = Fmt.day(d)
def weekend(d: String): String = { val w = new js.Date(d + "T00:00:00").getDay().toInt; if (w == 0 || w == 6) "num we" else "num" }
def tab(s: State, v: String) = s.view.mapC(x => if (x == v) "active" else "")
</script>

<template>
  <div class="page wide">
    <h1>作業時間の一覧</h1>
    <div class="row filters">
      <button class="small" @click="shift(s, -7)">← 前の週</button>
      <input type="date" v-model="s.from" @change="load(s)"> 〜 <input type="date" v-model="s.to" @change="load(s)">
      <button class="small" @click="shift(s, 7)">次の週 →</button>
      <button class="small" @click="thisMonth(s)">今月</button>
      <Combo :bind="Some(s.user)" :live="Some(s.users.mapC(Opt.users))" clearLabel="全員" :onChange="_ => load(s)" />
      <Combo :bind="Some(s.project)" :live="Some(Store.projects.mapC(_.map(p => Opt(p.id.toString, p.key + &quot; &quot; + p.name))))" clearLabel="すべてのプロジェクト"
        :onChange="_ => load(s)" />
      <span class="grow"></span>
      <b>合計 {{ total(s) }}</b>
    </div>
    <nav class="tabs">
      <button :class='tab(s, "table")' @click='setView(s, "table")'>人×日の表</button>
      <button :class='tab(s, "person")' @click='setView(s, "person")'>メンバー別（タスク×日）</button>
      <button :class='tab(s, "daily")' @click='setView(s, "daily")'>日ごとの明細</button>
    </nav>
    <div v-if='s.view.mapC(_ == "table")' class="table-wrap">
      <table class="list sheet">
        <thead><tr><th>メンバー / タスク</th><th v-for="d in days(s)" :class="weekend(d)">{{ dayHead(d) }}</th><th class="num">合計(h)</th></tr></thead>
        <tbody>
          <template v-for="p in people(s)">
            <tr class="person"><td><Avatar :user="Some(p._1)" :size="20" /> <b>{{ p._1.name }}</b></td>
              <td v-for="m in p._2" class="num"><b>{{ cell(m) }}</b></td><td class="num"><b>{{ Minutes.hours(p._3) }}</b></td></tr>
            <tr v-for="t in tasksOf(s, p._1.id)">
              <td class="task"><RouterLink :to='"/tasks/" + t._1.taskKey' class="key">{{ t._1.taskKey }}</RouterLink> {{ t._1.taskTitle }}</td>
              <td v-for="dm in dayIdx(s)" :class="cellClass(t._2(dm._2), dm._1)" :title="notes(s, t._1, dm._1)" @click="openCell(s, t._1, dm._1)">{{ cell(t._2(dm._2)) }}</td>
              <td class="num">{{ Minutes.hours(t._3) }}</td>
            </tr>
          </template>
          <tr class="totals"><td>日の合計</td><td v-for="m in dayTotals(s)" class="num">{{ cell(m) }}</td><td class="num">{{ total(s) }}</td></tr>
        </tbody>
      </table>
    </div>
    <div v-if='s.view.mapC(_ == "person")' class="table-wrap">
      <table class="list sheet">
        <thead><tr><th>タスク</th><th v-for="d in days(s)" :class="weekend(d)">{{ dayHead(d) }}</th><th class="num">合計(h)</th></tr></thead>
        <tbody>
          <tr v-for="t in personTasks(s)">
            <td class="ptask"><RouterLink :to='"/tasks/" + t._1.taskKey' class="key">{{ t._1.taskKey }}</RouterLink> {{ t._1.taskTitle }}</td>
            <td v-for="dm in dayIdx(s)" :class="cellClass(t._2(dm._2), dm._1)" :title="notes(s, t._1, dm._1)" @click="openCell(s, t._1, dm._1)">{{ cell(t._2(dm._2)) }}</td>
            <td class="num"><b>{{ Minutes.hours(t._3) }}</b></td>
          </tr>
          <tr class="totals"><td>日の合計</td><td v-for="m in personDays(s)" class="num">{{ cell(m) }}</td><td class="num">{{ personTotal(s) }}</td></tr>
        </tbody>
      </table>
    </div>
    <div v-if='s.view.mapC(_ == "daily")'>
      <section v-for="d in byDay(s)" class="day">
        <h3>{{ Fmt.day(d._1) }} <span class="muted small">{{ Minutes.format(d._2.map(_.minutes).sum) }}</span></h3>
        <div v-for="w in d._2" class="wrow">
          <Avatar :user="Some(w.user)" :size="22" />
          <span class="who">{{ w.user.name }}</span>
          <a href="#" class="mins" title="クリックで編集" @click.prevent="openLog(s, w)"><b>{{ Minutes.format(w.minutes) }}</b></a>
          <RouterLink :to='"/tasks/" + w.taskKey' class="key">{{ w.taskKey }}</RouterLink>
          <span class="grow">{{ w.taskTitle }}</span>
          <span class="muted small">{{ w.note }}</span>
        </div>
      </section>
    </div>
    <p v-if="s.list.mapC(_.isEmpty)" class="empty">この期間の作業時間の記録はありません</p>
    <WorklogEditor v-for="e in s.editing.mapC(_.toList)" :logs="e.logs" :title="e.title" :addTo="e.addTo"
      :onClose="_ => s.editing := None" :onSaved="_ => load(s)" />
  </div>
</template>

<style scoped>
.filters { margin-bottom: .8rem; }
.sheet td, .sheet th { white-space: nowrap; }
.num { text-align: right; width: 4.2em; }
.cellbtn { cursor: pointer; text-align: right; }
.cellbtn:hover { background: var(--accent-weak); outline: 1px solid var(--accent); outline-offset: -1px; }
.cellbtn.has { color: var(--fg); }
.we { background: color-mix(in srgb, var(--hover) 80%, transparent); }
.person td { background: var(--side); }
.ptask { max-width: 360px; overflow: hidden; text-overflow: ellipsis; }
.task { padding-left: 2rem !important; max-width: 360px; overflow: hidden; text-overflow: ellipsis; }
.totals td { font-weight: 700; border-top: 2px solid var(--line); }
.day { margin-bottom: 1rem; }
.wrow { display: flex; gap: .6rem; align-items: center; padding: .35rem 0; border-bottom: 1px solid var(--line); font-size: .88rem; flex-wrap: wrap; }
.who { width: 7em; }
.mins { width: 4.5em; }
</style>
