<script lang="scala">
import skea.web.*
import io.circe.Json
import io.circe.syntax.*
import org.scalajs.dom
import skea.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.util.{Failure, Success}

// タスクの検索（絞り込み構文・条件の欄・保存した条件・一括変更・CSV）. projectKey があればそのプロジェクトに絞る.
// 条件の欄（担当者・ステータス・日付など）は検索欄の絞り込み構文を書き換える（検索欄に直接書いた条件も欄に反映する）.
case class Props(projectKey: Option[String] = None, initial: String = "")

case class State(q: RV[String] = RV(""),
                 result: RV[Option[SearchResultDto]] = RV(None),
                 loading: RV[Boolean] = RV(false),
                 limit: RV[Int] = RV(100),
                 saved: RV[List[SavedFilterDto]] = RV(Nil),
                 selected: RV[Set[UUID]] = RV(Set.empty),
                 help: RV[Boolean] = RV(false),
                 bulkStatus: RV[String] = RV(""),
                 bulkAssignee: RV[String] = RV(""),
                 statuses: RV[Map[UUID, (String, String)]] = RV(Map.empty),
                 users: RV[List[UserBrief]] = RV(Nil),
                 conds: RV[Boolean] = RV(true),
                 cols: RV[List[String]] = RV(Prefs.get("list.cols", "key,type,title,status,priority,assignee,due,updated").split(",").toList.filter(_.nonEmpty)),
                 group: RV[String] = RV(Prefs.get("list.group", "")),
                 edit: RV[Boolean] = RV(false),
                 colsOpen: RV[Boolean] = RV(false),
                 projStatuses: RV[Map[UUID, List[StatusDto]]] = RV(Map.empty))

def initState(props: Props) = State(q = RV(props.initial))

def fullQuery(s: State, props: Props): String =
  (props.projectKey.map(k => s"project:$k").toList :+ s.q.v.trim).filter(_.nonEmpty).mkString(" ")

def run(s: State, props: Props): Unit = {
  s.loading := true
  Api.search(fullQuery(s, props), None, s.limit.v).onComplete {
    case Success(r) => s.result := Some(r); s.loading := false; s.selected := Set.empty; loadStatuses(s)
    case Failure(e) => Store.error(e); s.loading := false
  }
  if (props.projectKey.isEmpty) dom.window.history.replaceState(null, "", "/search?q=" + Api.enc(s.q.v))
}

def mounted(s: State, props: Props): Unit = {
  run(s, props)
  if (props.projectKey.isEmpty) Api.users("").foreach(s.users := _)
  Api.filters().foreach(s.saved := _)
  watch(Store.tick, false)((_, _) => run(s, props))
}

def quick(s: State, props: Props, add: String): Unit = {
  val cur = s.q.v.trim
  s.q := (if (cur.contains(add)) cur.replace(add, "").trim else (cur + " " + add).trim)
  run(s, props)
}

// ---------------------------------------------------------------- 条件の欄（検索欄の構文との相互変換）

/** 空白で区切る（"..." は1語. 引用符は残す）. */
def tokens(q: String): List[String] = {
  val out = List.newBuilder[String]
  val cur = new StringBuilder
  var inQ = false
  q.foreach { c =>
    if (c == '"') { inQ = !inQ; cur.append(c) }
    else if ((c == ' ' || c == '　') && !inQ) { if (cur.nonEmpty) { out += cur.toString; cur.clear() } }
    else cur.append(c)
  }
  if (cur.nonEmpty) out += cur.toString
  out.result()
}

private val ops = List(">=", "<=", ":", "<", ">")

/** 語のキーと演算子と値（"status:進行中" → ("status", ":", "進行中")）. */
def parts(t: String): Option[(String, String, String)] = {
  val body = t.stripPrefix("-")
  ops.flatMap(op => Option(body.indexOf(op)).filter(_ > 0).map(i => (i, op))).sortBy(_._1).headOption.map { (i, op) =>
    (body.take(i).toLowerCase, op, body.drop(i + op.length).stripPrefix(""").stripSuffix("""))
  }
}

def quote(v: String): String = if (v.exists(c => c == ' ' || c == '　')) """ + v + """ else v

/** 検索欄のその条件の値（否定の語は除く）. */
def cond(s: State, key: String, op: String = ":") = s.q.mapC(q =>
  tokens(q).filterNot(_.startsWith("-")).flatMap(parts).find(p => p._1 == key && p._2 == op).map(_._3).getOrElse(""))

/** その条件を置き換えて検索し直す（値が空なら外す）. */
def setCond(s: State, props: Props, key: String, op: String, value: String): Unit = {
  val kept = tokens(s.q.v).filterNot(t => !t.startsWith("-") && parts(t).exists(p => p._1 == key && p._2 == op))
  s.q := (kept ++ Option(value.trim).filter(_.nonEmpty).map(v => s"$key$op${quote(v)}")).mkString(" ")
  run(s, props)
}

/** 日付の条件の簡単な指定（今日・7日以内など）. 範囲の両端を置き換える. */
def preset(s: State, props: Props, key: String, from: String, to: String): Unit = {
  val kept = tokens(s.q.v).filterNot(t => !t.startsWith("-") && parts(t).exists(p => p._1 == key && (p._2 == ">=" || p._2 == "<=")))
  s.q := (kept ++ Option(from).filter(_.nonEmpty).map(v => s"$key>=$v") ++ Option(to).filter(_.nonEmpty).map(v => s"$key<=$v")).mkString(" ")
  run(s, props)
}

/** 担当者・報告者の選択肢（値はユーザー名. 自分・未割り当てを先頭に）. */
def userOpts(s: State, props: Props, unassigned: Boolean) = computed {
  List(Opt("me", "自分", "👤")) ++ (if (unassigned) List(Opt("none", "未割り当て")) else Nil) ++
    userChoices(s, props).v.map(u => Opt(u.name, u.name, user = Some(u)))
}
val categoryOpts: List[Opt] = StatusCategory.all.map(c => Opt(c, StatusCategory.label(c)))
val orderOpts: List[Opt] = Opt.of(List("-updated" -> "更新が新しい順", "-created" -> "作成が新しい順", "created" -> "作成が古い順", "due" -> "期日が近い順",
  "-priority" -> "優先度が高い順", "rank" -> "バックログの順", "key" -> "ID 順"))
def userChoices(s: State, props: Props) = computed {
  if (props.projectKey.isDefined) Store.meta.v.toList.flatMap(_.members.map(_.user)) else s.users.v
}
def statusChoices(props: Props) = Store.meta.mapC(m => if (props.projectKey.isDefined) m.toList.flatMap(_.statuses.map(_.name)) else Nil)
def labelChoices(props: Props) = Store.meta.mapC(m => if (props.projectKey.isDefined) m.toList.flatMap(_.labels.map(_.name)) else Nil)
def hasConds(s: State) = s.q.mapC(q => tokens(q).exists(t => parts(t).isDefined))
def clearConds(s: State, props: Props): Unit = { s.q := tokens(s.q.v).filter(t => parts(t).isEmpty).mkString(" "); run(s, props) }
def today: String = Fmt.today()
def daysAgo(n: Int): String = Fmt.addDays(Fmt.today(), -n)

def save(s: State): Unit = {
  Dialog.prompt("この検索条件の名前", "", "保存") { name =>
    if (name.trim.nonEmpty)
      Store.report(Api.post[SavedFilterDto]("/api/filters", SavedFilterReq(name.trim, s.q.v).asJson))(f => s.saved := s.saved.v :+ f)
  }
}

def removeSaved(s: State, f: SavedFilterDto): Unit = Store.report(Api.delete(s"/api/filters/${f.id}"))(_ => s.saved := s.saved.v.filterNot(_.id == f.id))

def toggle(s: State, id: UUID, on: Boolean): Unit = s.selected := (if (on) s.selected.v + id else s.selected.v - id)
def toggleAll(s: State, on: Boolean): Unit = s.selected := (if (on) s.result.v.toList.flatMap(_.hits.map(_.task.id)).toSet else Set.empty)

def bulk(s: State, props: Props, patch: Json): Unit =
  Store.report(Api.post[Json]("/api/tasks/bulk", BulkUpdateReq(s.selected.v.toList, patch).asJson)) { r =>
    Store.toast(s"${r.hcursor.get[Int]("updated").getOrElse(0)} 件を更新しました")
    Store.changed()
    run(s, props)
  }

def bulkStatus(s: State, props: Props): Unit =
  if (s.bulkStatus.v.nonEmpty) {
    // ステータスはプロジェクトごとに違うので、名前で各タスクのプロジェクトのステータスに合わせる
    val name = s.bulkStatus.v
    val hits = s.result.v.toList.flatMap(_.hits).filter(h => s.selected.v.contains(h.task.id))
    hits.groupBy(_.task.projectId).foreach { (pid, hs) =>
      Api.meta(pid).foreach { m =>
        m.statuses.find(_.name == name).foreach { st =>
          Api.post[Json]("/api/tasks/bulk", BulkUpdateReq(hs.map(_.task.id), Json.obj("statusId" -> st.id.asJson)).asJson).foreach { _ =>
            Store.changed(); run(s, props)
          }
        }
      }
    }
    s.bulkStatus := ""
  }

def bulkAssignee(s: State, props: Props): Unit = if (s.bulkAssignee.v.nonEmpty) {
  bulk(s, props, Json.obj("assigneeId" -> (if (s.bulkAssignee.v == "none") Json.Null else Json.fromString(s.bulkAssignee.v))))
  s.bulkAssignee := ""
}

/** 表示中の結果を CSV（Excel で開けるよう BOM 付き）で保存する. */
def csv(s: State): Unit = {
  val head = List("ID", "種類", "タイトル", "ステータス", "優先度", "担当者", "報告者", "開始日", "期日", "作業時間(分)", "更新").map(Csv.cell).mkString(",")
  val rows = s.result.v.toList.flatMap(_.hits).map { h =>
    val t = h.task
    List(t.key, TaskType.label(t.taskType), t.title, s.statuses.v.get(t.statusId).map(_._1).getOrElse(""), Priority.label(t.priority), t.assignee.map(_.name).getOrElse(""), t.reporter.map(_.name).getOrElse(""), t.startDate.getOrElse(""),
      t.dueDate.getOrElse(""), t.spentMinutes.toString, Fmt.dateTime(t.updatedAt)).map(Csv.cell).mkString(",")
  }
  val blob = new dom.Blob(js.Array[dom.BlobPart]("﻿" + (head :: rows).mkString("\r\n")), new dom.BlobPropertyBag { `type` = "text/csv" })
  val a = dom.document.createElement("a").asInstanceOf[dom.HTMLAnchorElement]
  a.href = dom.URL.createObjectURL(blob)
  a.setAttribute("download", "skea-tasks.csv")
  a.click()
}

// ---------------------------------------------------------------- 表示の切り替え（列・グループ化・その場で編集）

val columns: List[(String, String)] = List("key" -> "ID", "type" -> "種類", "title" -> "タイトル", "status" -> "ステータス", "priority" -> "優先度",
  "assignee" -> "担当者", "reporter" -> "報告者", "labels" -> "ラベル", "start" -> "開始日", "due" -> "期日", "spent" -> "作業時間",
  "created" -> "作成", "updated" -> "更新")
val groupOpts: List[Opt] = Opt.of(List("" -> "グループ化しない", "status" -> "ステータスごと", "assignee" -> "担当者ごと", "priority" -> "優先度ごと",
  "type" -> "種類ごと", "project" -> "プロジェクトごと"))

def colLabel(c: String): String = columns.find(_._1 == c).map(_._2).getOrElse(c)
def visibleCols(s: State) = s.cols.mapC(cs => columns.map(_._1).filter(cs.contains))
def hasCol(s: State, c: String) = s.cols.mapC(_.contains(c))
def colSpan(s: State) = s.cols.mapC(cs => (cs.size + 1).toString)
def toggleCol(s: State, c: String, on: Boolean): Unit = {
  s.cols := (if (on) (s.cols.v :+ c).distinct else s.cols.v.filterNot(_ == c))
  Prefs.set("list.cols", s.cols.v.mkString(","))
}

/** グループ（見出し, 行）. 見出しが空ならグループ化しない. */
def groups(s: State) = computed {
  val hs = s.result.v.toList.flatMap(_.hits)
  s.group.v match {
    case "status" =>
      hs.groupBy(h => s.statuses.v.get(h.task.statusId).map(_._1).getOrElse("")).toList.sortBy(g => statusOrder(s, g._2.head.task))
    case "assignee" => hs.groupBy(_.task.assignee.map(_.name).getOrElse("未割り当て")).toList.sortBy(g => (g._1 == "未割り当て", g._1))
    case "priority" => hs.groupBy(h => Priority.icon(h.task.priority) + " " + Priority.label(h.task.priority)).toList
        .sortBy(g => -Priority.rank(g._2.head.task.priority))
    case "type"     => hs.groupBy(h => TaskType.icon(h.task.taskType) + " " + TaskType.label(h.task.taskType)).toList
        .sortBy(g => TaskType.all.indexOf(g._2.head.task.taskType))
    case "project"  => hs.groupBy(_.projectKey).toList.sortBy(_._1)
    case _          => List(("", hs))
  }
}

def statusOrder(s: State, t: TaskDto): (Int, String) = {
  val st = s.projStatuses.v.get(t.projectId).toList.flatten
  (StatusCategory.all.indexOf(st.find(_.id == t.statusId).map(_.category).getOrElse("")), st.find(_.id == t.statusId).map(_.position.toString).getOrElse(""))
}

def statusOptsOf(s: State, h: SearchHitDto) = s.projStatuses.mapC(m => Opt.statuses(m.getOrElse(h.task.projectId, Nil)))

def labelsOf(t: TaskDto): List[LabelDto] = Store.meta.v.toList.flatMap(_.labels).filter(l => t.labelIds.contains(l.id))

/** その場で編集: 1つのフィールドを変えて結果を取り直す. */
def edit(s: State, props: Props, h: SearchHitDto, kv: (String, Json)): Unit =
  Store.report(Api.updateTask(h.task.id, Json.obj(kv))) { _ => Store.changed(); run(s, props) }

def rowClick(s: State, h: SearchHitDto, c: String): Unit =
  if (!s.edit.v && c != "key" && c != "title") Router.go("/tasks/" + h.task.key)

def statusName(s: State, t: TaskDto) = s.statuses.mapC(_.get(t.statusId).map(_._1).getOrElse(""))
def statusCls(s: State, t: TaskDto) = s.statuses.mapC(m => "badge cat-" + m.get(t.statusId).map(_._2).getOrElse(""))

/** 結果に出てくるプロジェクトのステータス名を読む（一覧の表示用）. */
def loadStatuses(s: State): Unit = {
  val pids = s.result.v.toList.flatMap(_.hits.map(_.task.projectId)).distinct
  pids.foreach(pid => Api.get[List[StatusDto]](s"/api/projects/$pid/statuses").foreach { ss =>
    s.statuses := s.statuses.v ++ ss.map(x => x.id -> (x.name, x.category))
    s.projStatuses := s.projStatuses.v.updated(pid, ss)
  })
}

def hits(s: State) = s.result.mapC(_.map(_.hits).getOrElse(Nil))
def total(s: State) = s.result.mapC(_.map(r => s"${r.total} 件").getOrElse(""))
def error(s: State) = s.result.mapC(_.flatMap(_.error).getOrElse(""))
def more(s: State) = s.result.mapC(_.exists(r => r.total > r.hits.count(!_.fuzzy)))
def anySelected(s: State) = s.selected.mapC(_.nonEmpty)
def isSel(s: State, id: UUID) = s.selected.mapC(_.contains(id))
def statusNames = Store.meta.mapC(_.map(_.statuses.map(_.name)).getOrElse(List("作業前", "進行中", "待機中", "完了")))
def members = Store.meta.mapC(_.map(_.members.map(_.user)).getOrElse(Nil))
def chip(s: State, add: String) = s.q.mapC(q => if (q.contains(add)) "chip active" else "chip")
def overdue(t: TaskDto): Boolean = Fmt.isOverdue(t.dueDate, t.resolvedAt.isDefined)
</script>

<template>
  <div>
    <form class="row qrow" @submit.prevent="run(s, props)">
      <input type="search" v-model="s.q" class="grow" placeholder='検索語と絞り込み（例: ログイン status:進行中 assignee:me due<+7d）'>
      <button type="submit" class="primary">検索</button>
      <button type="button" class="ghost" @click="s.help := !s.help.v">？</button>
    </form>
    <div v-if="s.help" class="panel small help">
      <p>空白区切りの語はすべてを含むタスク（タイトル・説明・コメント）を探します。ひらがな・カタカナ、全角・半角の違いは区別せず、見つからないときは表記の揺れ・入力ミスを許して探します（「あいまい」の印）。</p>
      <ul>
        <li><code>project:SKEA</code> <code>status:進行中</code> <code>category:done</code>（todo / in_progress / waiting / done）</li>
        <li><code>assignee:me</code> <code>assignee:none</code> <code>reporter:山田</code> <code>watcher:me</code> <code>commented:me</code></li>
        <li><code>label:バグ</code> <code>sprint:active</code>（backlog / planned / スプリント名） <code>parent:SKEA-1</code> <code>key:SKEA-1,SKEA-2</code></li>
        <li><code>due&lt;2026-12-01</code> <code>due&lt;=+7d</code> <code>updated&gt;-7d</code> <code>created:today</code> <code>due:none</code></li>
        <li><code>is:open</code> <code>is:done</code> <code>is:overdue</code> <code>is:unassigned</code> <code>is:subtask</code> <code>is:watching</code></li>
        <li><code>has:attachment</code> <code>has:commits</code> <code>has:subtasks</code> <code>field:顧客=A社</code> <code>comment:確認</code></li>
        <li>先頭に <code>-</code> を付けると否定（<code>-label:保留</code>）。並び順は <code>order:due</code> <code>order:-updated</code>（- は降順）。値に空白を含めるときは "..." で囲みます。</li>
      </ul>
    </div>
    <div class="conds-head row small">
      <a href="#" @click.prevent="s.conds := !s.conds.v">{{ s.conds.mapC(c => if (c) "▾ 条件" else "▸ 条件") }}</a>
      <a v-if="hasConds(s)" href="#" class="muted" @click.prevent="clearConds(s, props)">条件をすべて外す</a>
    </div>
    <div v-if="s.conds" class="conds">
      <div class="cf"><span>担当者</span>
        <Combo :live="Some(userOpts(s, props, true))" :shows='Some(cond(s, "assignee"))' clearLabel="指定なし" :small="true"
          :onChange='v => setCond(s, props, "assignee", ":", v)' />
      </div>
      <div class="cf"><span>報告者</span>
        <Combo :live="Some(userOpts(s, props, false))" :shows='Some(cond(s, "reporter"))' clearLabel="指定なし" :small="true"
          :onChange='v => setCond(s, props, "reporter", ":", v)' />
      </div>
      <div class="cf"><span>ステータス</span>
        <Combo :live="Some(statusChoices(props).mapC(Opt.names))" :shows='Some(cond(s, "status"))' clearLabel="指定なし" :small="true"
          :onChange='v => setCond(s, props, "status", ":", v)' />
      </div>
      <div class="cf"><span>状態</span>
        <Combo :options="categoryOpts" :shows='Some(cond(s, "category"))' clearLabel="指定なし" :small="true"
          :onChange='v => setCond(s, props, "category", ":", v)' />
      </div>
      <div class="cf"><span>種類</span>
        <Combo :options="Opt.types" :shows='Some(cond(s, "type"))' clearLabel="指定なし" :small="true" :onChange='v => setCond(s, props, "type", ":", v)' />
      </div>
      <div class="cf"><span>優先度</span>
        <Combo :options="Opt.priorities" :shows='Some(cond(s, "priority"))' clearLabel="指定なし" :small="true"
          :onChange='v => setCond(s, props, "priority", ":", v)' />
      </div>
      <div v-if="labelChoices(props).mapC(_.nonEmpty)" class="cf"><span>ラベル</span>
        <Combo :live="Some(labelChoices(props).mapC(Opt.names))" :shows='Some(cond(s, "label"))' clearLabel="指定なし" :small="true"
          :onChange='v => setCond(s, props, "label", ":", v)' />
      </div>
      <div class="cf date"><span>作成日</span>
        <div class="row nowrap">
          <input type="date" :value='cond(s, "created", ">=")' @change='setCond(s, props, "created", ">=", Dom.value(event))'>〜
          <input type="date" :value='cond(s, "created", "<=")' @change='setCond(s, props, "created", "<=", Dom.value(event))'>
        </div>
        <div class="presets"><a href="#" @click.prevent='preset(s, props, "created", today, "")'>今日</a>
          <a href="#" @click.prevent='preset(s, props, "created", daysAgo(7), "")'>7日以内</a>
          <a href="#" @click.prevent='preset(s, props, "created", daysAgo(30), "")'>30日以内</a></div>
      </div>
      <div class="cf date"><span>更新日</span>
        <div class="row nowrap">
          <input type="date" :value='cond(s, "updated", ">=")' @change='setCond(s, props, "updated", ">=", Dom.value(event))'>〜
          <input type="date" :value='cond(s, "updated", "<=")' @change='setCond(s, props, "updated", "<=", Dom.value(event))'>
        </div>
        <div class="presets"><a href="#" @click.prevent='preset(s, props, "updated", today, "")'>今日</a>
          <a href="#" @click.prevent='preset(s, props, "updated", daysAgo(7), "")'>7日以内</a>
          <a href="#" @click.prevent='preset(s, props, "updated", daysAgo(30), "")'>30日以内</a></div>
      </div>
      <div class="cf date"><span>期日</span>
        <div class="row nowrap">
          <input type="date" :value='cond(s, "due", ">=")' @change='setCond(s, props, "due", ">=", Dom.value(event))'>〜
          <input type="date" :value='cond(s, "due", "<=")' @change='setCond(s, props, "due", "<=", Dom.value(event))'>
        </div>
        <div class="presets"><a href="#" @click.prevent='preset(s, props, "due", "", today)'>今日まで</a>
          <a href="#" @click.prevent='preset(s, props, "due", today, Fmt.addDays(today, 7))'>7日以内</a></div>
      </div>
      <div class="cf"><span>並び順</span>
        <Combo :options="orderOpts" :shows='Some(cond(s, "order"))' clearLabel="関連度・既定" :small="true" :onChange='v => setCond(s, props, "order", ":", v)' />
      </div>
    </div>
    <div class="row chips-row">
      <button :class='chip(s, "assignee:me")' @click='quick(s, props, "assignee:me")'>自分の担当</button>
      <button :class='chip(s, "is:open")' @click='quick(s, props, "is:open")'>未完了</button>
      <button :class='chip(s, "is:overdue")' @click='quick(s, props, "is:overdue")'>期限切れ</button>
      <button :class='chip(s, "updated>-7d")' @click='quick(s, props, "updated>-7d")'>1週間以内に更新</button>
      <button :class='chip(s, "is:watching")' @click='quick(s, props, "is:watching")'>ウォッチ中</button>
      <span class="grow"></span>
      <button class="small" @click="save(s)">条件を保存</button>
      <button class="small" @click="csv(s)">CSV</button>
    </div>
    <div v-if="s.saved.mapC(_.nonEmpty)" class="row saved small">
      <span class="muted">保存した条件:</span>
      <span v-for="f in s.saved" :key="f.id" class="saved-item">
        <a href="#" @click.prevent="s.q := f.query; run(s, props)">{{ f.name }}</a><button class="ghost small" @click="removeSaved(s, f)">×</button>
      </span>
    </div>
    <p v-if="error(s).mapC(_.nonEmpty)" class="error">{{ error(s) }}</p>
    <div v-if="anySelected(s)" class="row bulk">
      <span class="small">{{ s.selected.mapC(_.size.toString) }} 件を選択:</span>
      <Combo :live="Some(statusNames.mapC(Opt.names))" :keep="false" :small="true" placeholder="ステータスを変更…"
        :onChange="v => { s.bulkStatus := v; bulkStatus(s, props) }" />
      <Combo :live='Some(members.mapC(us => Opt("none", "未割り当て") :: Opt.users(us)))' :keep="false" :small="true" placeholder="担当者を変更…"
        :onChange="v => { s.bulkAssignee := v; bulkAssignee(s, props) }" />
    </div>
    <p class="muted small">{{ total(s) }} {{ s.loading.mapC(l => if (l) "検索中…" else "") }}</p>
    <div class="row view-row small">
      <span class="muted">表示:</span>
      <Combo :options="groupOpts" :bind="Some(s.group)" :small="true" :onChange='v => Prefs.set("list.group", v)' />
      <div class="cols-wrap">
        <button class="small" @click="s.colsOpen := !s.colsOpen.v">列を選ぶ ▾</button>
        <div v-if="s.colsOpen" class="cols-menu" @mouseleave="s.colsOpen := false">
          <label v-for="c in columns" class="small"><input type="checkbox" :checked="hasCol(s, c._1)" @change="toggleCol(s, c._1, Dom.checked(event))"> {{ c._2 }}</label>
        </div>
      </div>
      <label class="small"><input type="checkbox" v-model="s.edit"> その場で編集</label>
    </div>
    <div class="table-wrap">
      <table class="list">
        <thead><tr>
          <th><input type="checkbox" @change="toggleAll(s, Dom.checked(event))"></th>
          <th v-for="c in visibleCols(s)">{{ colLabel(c) }}</th>
        </tr></thead>
        <tbody v-for="g in groups(s)">
          <tr v-if="g._1.nonEmpty" class="ghead"><td :colspan="colSpan(s)"><b>{{ g._1 }}</b> <span class="muted small">{{ g._2.size }} 件</span></td></tr>
          <tr v-for="h in g._2" :key="h.task.id" :class='s.edit.mapC(e => if (e) "" else "clickable")'>
            <td><input type="checkbox" :checked="isSel(s, h.task.id)" @change="toggle(s, h.task.id, Dom.checked(event))"></td>
            <td v-for="c in visibleCols(s)" :class='"c-" + c' @click="rowClick(s, h, c)">
              <RouterLink v-if='c == "key"' :to='"/tasks/" + h.task.key' class="key">{{ h.task.key }}</RouterLink>
              <template v-if='c == "title"'>
                <RouterLink :to='"/tasks/" + h.task.key' class="ttl">{{ h.task.title }}</RouterLink> <span v-if="h.fuzzy" class="badge">あいまい</span>
                <div v-if="h.snippet.isDefined" class="muted small snippet" v-html='h.snippet.getOrElse("")'></div>
              </template>
              <template v-if='c == "type"'>
                <Combo v-if="s.edit" :options="Opt.types" :value="h.task.taskType" :small="true" :onChange='v => edit(s, props, h, "taskType" -> Json.fromString(v))' />
                <span v-else :title="TaskType.label(h.task.taskType)">{{ TaskType.icon(h.task.taskType) }} {{ TaskType.label(h.task.taskType) }}</span>
              </template>
              <template v-if='c == "status"'>
                <Combo v-if="s.edit" :live="Some(statusOptsOf(s, h))" :value="h.task.statusId.toString" :small="true"
                  :onChange='v => edit(s, props, h, "statusId" -> Json.fromString(v))' />
                <span v-else :class="statusCls(s, h.task)">{{ statusName(s, h.task) }}</span>
              </template>
              <template v-if='c == "priority"'>
                <Combo v-if="s.edit" :options="Opt.priorities" :value="h.task.priority" :small="true" :onChange='v => edit(s, props, h, "priority" -> Json.fromString(v))' />
                <span v-else>{{ Priority.icon(h.task.priority) }} {{ Priority.label(h.task.priority) }}</span>
              </template>
              <template v-if='c == "assignee"'>
                <Combo v-if="s.edit" :live="Some(userChoices(s, props).mapC(Opt.users))" :value='h.task.assignee.map(_.id.toString).getOrElse("")' clearLabel="未割り当て"
                  :small="true" :onChange='v => edit(s, props, h, "assigneeId" -> (if (v.isEmpty) Json.Null else Json.fromString(v)))' />
                <span v-else class="nowrap"><Avatar :user="h.task.assignee" :size="20" /> {{ h.task.assignee.map(_.name).getOrElse("") }}</span>
              </template>
              <span v-if='c == "reporter"' class="nowrap">{{ h.task.reporter.map(_.name).getOrElse("") }}</span>
              <span v-if='c == "labels"'><span v-for="l in labelsOf(h.task)" class="label-chip" :style='"background:" + l.color'>{{ l.name }}</span></span>
              <template v-if='c == "start" || c == "due"'>
                <input v-if="s.edit" type="date" class="date-in" :value='(if (c == "due") h.task.dueDate else h.task.startDate).getOrElse("")'
                  @change='edit(s, props, h, (if (c == "due") "dueDate" else "startDate") -> (if (Dom.value(event).isEmpty) Json.Null else Json.fromString(Dom.value(event))))'>
                <span v-else :class='if (c == "due" && overdue(h.task)) "nowrap small overdue" else "nowrap small"'>{{ (if (c == "due") h.task.dueDate else h.task.startDate).map(Fmt.day).getOrElse("") }}</span>
              </template>
              <span v-if='c == "spent"' class="small">{{ if (h.task.spentMinutes > 0) Minutes.format(h.task.spentMinutes) else "" }}</span>
              <span v-if='c == "created"' class="muted small nowrap">{{ Fmt.ago(h.task.createdAt) }}</span>
              <span v-if='c == "updated"' class="muted small nowrap">{{ Fmt.ago(h.task.updatedAt) }}</span>
            </td>
          </tr>
        </tbody>
      </table>
    </div>
    <p v-if="more(s)"><button @click="s.limit := s.limit.v + 100; run(s, props)">もっと見る</button></p>
  </div>
</template>

<style scoped>
.qrow { margin-bottom: .4rem; }
.conds-head { gap: 1rem; margin-bottom: .3rem; }
.conds { display: flex !important; flex-wrap: wrap; gap: .5rem .9rem; align-items: flex-start; background: var(--side); border: 1px solid var(--line);
  border-radius: 8px; padding: .6rem .8rem; margin-bottom: .6rem; }
.cf { display: flex; flex-direction: column; gap: .15rem; font-size: .82rem; }
.cf > span { font-size: .75rem; color: var(--muted); }
.cf .combo { min-width: 9rem; }
.cf.date input { width: 9.6rem; font-size: .85rem; padding: .25rem .4rem; }
.presets { display: flex; gap: .6rem; font-size: .75rem; }
.chips-row { margin-bottom: .5rem; }
.saved { margin-bottom: .5rem; gap: .3rem; }
.saved-item { display: inline-flex; align-items: center; border: 1px solid var(--line); border-radius: 999px; padding: 0 .2rem 0 .6rem; }
.bulk { background: var(--accent-weak); border-radius: 8px; padding: .4rem .6rem; margin-bottom: .5rem; }
.view-row { margin: .2rem 0 .5rem; gap: .6rem; }
.cols-wrap { position: relative; }
.cols-menu { position: absolute; top: 2rem; left: 0; z-index: 50; background: var(--surface); border: 1px solid var(--line); border-radius: 9px;
  padding: .5rem .7rem; display: grid; grid-template-columns: 1fr 1fr; gap: .2rem 1rem; min-width: 260px; box-shadow: var(--shadow); }
.ghead td { background: var(--side); }
.c-title { min-width: 16rem; }
.ttl { color: var(--fg); }
.date-in { font-size: .8rem; padding: .15rem .3rem; }
td .combo { min-width: 7.5rem; }
.help ul { margin: .3rem 0; padding-left: 1.2rem; }
.help li { margin: .2rem 0; }
</style>
