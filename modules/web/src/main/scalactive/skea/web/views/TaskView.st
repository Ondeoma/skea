<script lang="scala">
import skea.web.*
import io.circe.Json
import io.circe.syntax.*
import org.scalajs.dom
import skea.shared.*
import skea.web.parts.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

// タスクの詳細. modal なら、カンバンなどの上にモーダルで出す（閉じると onClose）.
// 変更の同期で詳細を読み直しても、コメント・説明の入力欄は作り直さない（表示中のタスクが変わったときだけ作り直す）.
case class Props(taskKey: String, modal: Boolean = false, onClose: Unit => Unit = _ => ())

/** 表示中のタスク（コメント欄・説明の入力欄はこれが変わったときだけ作り直す）. */
case class Ident(id: UUID, key: String, projectId: UUID, canEdit: Boolean, isAdmin: Boolean)

case class State(detail: RV[Option[TaskDetailDto]] = RV(None),
                 audience: RV[List[UserBrief]] = RV(Nil),
                 error: RV[String] = RV(""),
                 editTitle: RV[Boolean] = RV(false),
                 title: RV[String] = RV(""),
                 editDesc: RV[Boolean] = RV(false),
                 descEditor: RV[Option[EditorHandle]] = RV(None),
                 stale: RV[Boolean] = RV(false),
                 tab: RV[String] = RV("comments"),
                 linkKind: RV[String] = RV(LinkKind.Relates),
                 linkTarget: RV[String] = RV(""),
                 linkTitle: RV[String] = RV(""),
                 addingLink: RV[Boolean] = RV(false),
                 depKey: RV[String] = RV(""),
                 newLabel: RV[String] = RV(""),
                 ident: RV[Option[Ident]] = RV(None),
                 descContent: RV[Option[Json]] = RV(None),
                 rules: RV[List[ManualRuleDto]] = RV(Nil),
                 uploading: RV[Boolean] = RV(false))

def initState() = State()

def load(s: State, props: Props): Unit =
  Api.task(props.taskKey).onComplete {
    case Success(d) =>
      s.detail := Some(d)
      val id = Ident(d.task.id, d.task.key, d.task.projectId, Roles.rank(d.role) >= 2, d.role == Roles.Admin)
      if (!s.ident.v.contains(id)) s.ident := Some(id)
      s.stale := false
      if (!props.modal) pageTitle(s"${d.task.key} ${d.task.title}")
    case Failure(e) => s.error := e.getMessage
  }

def mounted(s: State, props: Props): Unit = {
  load(s, props)
  Store.openProject(props.taskKey.takeWhile(_ != '-').toUpperCase).foreach { m =>
    Store.audience(m.project.id).foreach(s.audience := _)
  }
  Api.task(props.taskKey).foreach(d => Api.manualRules(d.task.id).foreach(s.rules := _))
  watch(Store.tick, false)((_, _) => if (s.editDesc.v || s.editTitle.v) s.stale := true else load(s, props))
}

def task(s: State) = s.detail.mapC(_.map(_.task))
def t(s: State): Option[TaskDto] = s.detail.v.map(_.task)

/** 変えるフィールドだけを送る（version で同時更新を検出）. */
def patch(s: State, props: Props, kv: (String, Json)*): Unit = s.detail.v.foreach { d =>
  Api.updateTask(d.task.id, Json.obj((kv :+ ("version" -> d.task.version.asJson))*)).onComplete {
    case Success(_) => load(s, props); Store.changed()
    case Failure(e) => Store.error(e); load(s, props)
  }
}

def opt(v: String): Json = if (v.isEmpty) Json.Null else Json.fromString(v)

def saveTitle(s: State, props: Props): Unit = {
  s.editTitle := false
  if (s.title.v.trim.nonEmpty && t(s).exists(_.title != s.title.v.trim)) patch(s, props, "title" -> s.title.v.trim.asJson)
}

def editDesc(s: State): Unit = if (canEdit(s).v) {
  s.descContent := s.detail.v.flatMap(_.description)
  s.editDesc := true
}

def saveDesc(s: State, props: Props): Unit = s.descEditor.v.foreach { ed =>
  s.editDesc := false
  patch(s, props, "description" -> (if (ed.isEmpty()) Json.Null else Editor.fromJs(ed.getJSON())))
}

def toggleLabel(s: State, props: Props, l: LabelDto, on: Boolean): Unit = t(s).foreach { x =>
  val ids = if (on) (x.labelIds :+ l.id).distinct else x.labelIds.filterNot(_ == l.id)
  patch(s, props, "labelIds" -> ids.map(_.toString).asJson)
}

def addLabel(s: State, props: Props): Unit = (Store.meta.v, t(s)) match {
  case (Some(m), Some(x)) if s.newLabel.v.trim.nonEmpty =>
    Store.report(Api.post[LabelDto](s"/api/projects/${m.project.id}/labels", LabelReq(s.newLabel.v.trim, None).asJson)) { l =>
      s.newLabel := ""
      Store.reloadMeta()
      patch(s, props, "labelIds" -> (x.labelIds :+ l.id).distinct.map(_.toString).asJson)
    }
  case _ => ()
}

def setField(s: State, props: Props, f: CustomFieldDto, v: Json): Unit = patch(s, props, "fields" -> Json.obj(f.id.toString -> v))
def watchToggle(s: State, props: Props): Unit = s.detail.v.foreach { d =>
  val f = if (d.watching) Api.delete(s"/api/tasks/${d.task.id}/watch") else Api.postUnit(s"/api/tasks/${d.task.id}/watch")
  Store.report(f)(_ => load(s, props))
}

def addWatcher(s: State, props: Props, uid: String): Unit = s.detail.v.foreach { d =>
  if (uid.nonEmpty) Store.report(Api.addWatcher(d.task.id, UUID.fromString(uid)))(_ => load(s, props))
}

def removeWatcher(s: State, props: Props, u: UserBrief): Unit = s.detail.v.foreach { d =>
  Store.report(Api.removeWatcher(d.task.id, u.id))(_ => load(s, props))
}

/** ウォッチャーに追加できる人（プロジェクトのメンバーのうち、まだウォッチしていない人）. */
def watcherCandidates(s: State) = computed {
  val ws = s.detail.v.toList.flatMap(_.watchers.map(_.id)).toSet
  s.audience.v.filterNot(u => ws(u.id))
}

def addLink(s: State, props: Props): Unit = t(s).foreach { x =>
  val target = s.linkTarget.v.trim
  val body =
    if (s.linkKind.v == LinkKind.Web) CreateLinkReq(LinkKind.Web, None, Some(target), Some(s.linkTitle.v.trim).filter(_.nonEmpty))
    else CreateLinkReq(s.linkKind.v, Some(target), None, None)
  Store.report(Api.postUnit(s"/api/tasks/${x.id}/links", body.asJson)) { _ =>
    s.linkTarget := ""; s.linkTitle := ""; s.addingLink := false
    load(s, props)
  }
}

def removeLink(s: State, props: Props, l: TaskLinkDto): Unit = t(s).foreach { x =>
  Store.report(Api.delete(s"/api/tasks/${x.id}/links/${l.id}"))(_ => load(s, props))
}

def addDep(s: State, props: Props): Unit = t(s).foreach { x =>
  Store.report(Api.postUnit(s"/api/tasks/${x.id}/dependencies", CreateDependencyReq(s.depKey.v.trim.toUpperCase, None).asJson)) { _ =>
    s.depKey := ""
    load(s, props)
  }
}

def removeDep(s: State, props: Props, r: TaskRef): Unit = t(s).foreach { x =>
  Store.report(Api.delete(s"/api/tasks/${x.id}/dependencies/${r.id}"))(_ => load(s, props))
}

def upload(s: State, props: Props, e: dom.Event): Unit = t(s).foreach { x =>
  val input = e.target.asInstanceOf[dom.HTMLInputElement]
  val files = (0 until input.files.length).map(input.files(_)).toList
  s.uploading := true
  files.foldLeft(scala.concurrent.Future.successful(())) { (acc, f) => acc.flatMap(_ => Api.upload(x.id, f).map(_ => ())) }.onComplete { r =>
    s.uploading := false
    input.value = ""
    r.failed.foreach(Store.error)
    load(s, props)
    Store.changed()
  }
}

def removeAttachment(s: State, props: Props, a: AttachmentDto): Unit =
  Dialog.confirm(s"「${a.fileName}」を削除しますか？", "削除する") { Store.report(Api.delete(s"/api/attachments/${a.id}"))(_ => load(s, props)) }

def runRule(s: State, r: ManualRuleDto): Unit = t(s).foreach { x =>
  Store.report(Api.postUnit(s"/api/tasks/${x.id}/automations/${r.id}/run")) { _ =>
    Store.toast(s"「${r.name}」を実行しました（数秒で反映されます）")
    dom.window.setTimeout(() => Store.changed(), 3000)
  }
}

def deleteTask(s: State, props: Props): Unit = t(s).foreach { x =>
  Dialog.confirm(s"${x.key}「${x.title}」を削除しますか？サブタスクも削除されます。", "削除する") {
    Store.report(Api.deleteTask(x.id)) { _ =>
      Store.toast(s"${x.key} を削除しました")
      Store.changed()
      if (props.modal) props.onClose(()) else Router.go(s"/p/${x.key.takeWhile(_ != '-')}/board")
    }
  }
}

def addSubtask(s: State): Unit = t(s).foreach(x => Store.createDialog := Some((x.projectId, Some(x.id))))

def copyKey(s: State): Unit = t(s).foreach { x =>
  dom.window.navigator.clipboard.writeText(s"${x.key} ${x.title}")
  Store.toast("タスクIDとタイトルをコピーしました")
}

def canEdit(s: State) = s.detail.mapC(_.exists(d => Roles.rank(d.role) >= 2))
def isAdmin(s: State) = s.detail.mapC(_.exists(_.role == Roles.Admin))
def statuses = Store.meta.mapC(_.map(_.statuses).getOrElse(Nil))
def sprints = Store.meta.mapC(_.map(_.sprints).getOrElse(Nil))
def sprintOpts = sprints.mapC(_.map(sp => Opt(sp.id.toString, sp.name, hint = if (sp.state == "active") "進行中" else "")))
val linkKinds: List[Opt] = LinkKind.taskKinds.map(k => Opt(k, LinkKind.label(k))) :+ Opt(LinkKind.Web, "Webリンク（URL）")
def labels = Store.meta.mapC(_.map(_.labels).getOrElse(Nil))
def fields = Store.meta.mapC(_.map(_.fields).getOrElse(Nil))
def statusOf(s: State) = computed { s.detail.v.flatMap(d => Store.meta.v.flatMap(_.statuses.find(_.id == d.task.statusId))) }
def overdue(s: State) = computed { s.detail.v.exists(d => Fmt.isOverdue(d.task.dueDate, statusOf(s).v.exists(_.category == StatusCategory.Done))) }
def tabClass(s: State, name: String) = s.tab.mapC(x => if (x == name) "active" else "")
def projectLink(s: State) = s.detail.mapC(_.map(d => "/p/" + d.task.key.takeWhile(_ != '-') + "/board").getOrElse("/"))
def isWeb(s: State) = s.linkKind.mapC(_ == LinkKind.Web)
def spent(s: State) = s.detail.mapC(_.map(d => Minutes.format(d.task.spentMinutes)).getOrElse(""))
def descHtml(s: State) = s.detail.mapC(_.map(_.descriptionHtml).getOrElse(""))
def noDesc(s: State) = computed { !s.editDesc.v && s.detail.v.exists(_.descriptionHtml.isEmpty) }
def pageClass(props: Props): String = if (props.modal) "tv in-modal" else "page tv"
def catOf(id: UUID): String = Fmt.statusCat(Store.meta.v, id)
def nameOf(id: UUID): String = Fmt.statusName(Store.meta.v, id)
</script>

<template>
  <div :class="pageClass(props)">
    <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
    <template v-for="d in s.detail.mapC(_.toList)">
      <div class="crumbs small row">
        <RouterLink :to="projectLink(s).v">{{ d.task.key.takeWhile(_ != '-') }}</RouterLink>
        <template v-for="a in d.ancestors"> / <RouterLink :to='"/tasks/" + a.key'>{{ a.key }}</RouterLink></template>
        / <a href="#" class="key" @click.prevent="copyKey(s)" title="コピー">{{ d.task.key }}</a>
        <span class="grow"></span>
        <RouterLink v-if="props.modal" :to='"/tasks/" + d.task.key' class="small">↗ 画面で開く</RouterLink>
        <button v-if="props.modal" class="ghost small" @click="props.onClose(())" title="閉じる（Esc）">✕</button>
      </div>
      <div v-if="s.stale" class="stale small">ほかの人がこのタスクを更新しました。<a href="#" @click.prevent="load(s, props)">最新の内容を読み込む</a></div>
    </template>
    <div v-if="s.ident.mapC(_.isDefined)" class="layout">
      <div class="main">
        <template v-for="d in s.detail.mapC(_.toList)">
          <h1 v-if="s.editTitle.mapC(!_)" class="title" @click="if (canEdit(s).v) { s.title := d.task.title; s.editTitle := true }">{{ d.task.title }}</h1>
          <form v-if="s.editTitle" @submit.prevent="saveTitle(s, props)" class="row">
            <input type="text" v-model="s.title" class="grow title-input" autofocus @keydown.esc="s.editTitle := false">
            <button type="submit" class="primary small">保存</button><button type="button" class="small" @click="s.editTitle := false">キャンセル</button>
          </form>
          <div class="toolbar row">
            <button v-if="canEdit(s)" class="small" @click="addSubtask(s)">＋ サブタスク</button>
            <button v-if="canEdit(s)" class="small" @click="s.addingLink := !s.addingLink.v">🔗 リンク</button>
            <label v-if="canEdit(s)" class="btn small">📎 添付<input type="file" multiple class="hidden" @change="upload(s, props, event)"></label>
            <span v-if="s.uploading" class="muted small">アップロード中…</span>
          </div>
        </template>

        <section v-for="i in s.ident.mapC(_.toList)">
          <h3>説明 <button v-if="computed { canEdit(s).v && !s.editDesc.v }" class="ghost small" @click="editDesc(s)">編集</button></h3>
          <div v-if="s.editDesc.mapC(!_)" class="rich desc" v-html="descHtml(s)" @dblclick="editDesc(s)"></div>
          <p v-if="noDesc(s)" class="muted small">説明はありません</p>
          <div v-if="s.editDesc">
            <RichEditor :content="s.descContent.v" :projectId="Some(i.projectId)" :taskId="Some(i.id)" :autofocus="true"
              placeholder="詳しい内容（@でメンション、/でメニュー、画像は貼り付けで添付）" :onReady="h => s.descEditor := Some(h)" :onSubmit="_ => saveDesc(s, props)" />
            <div class="row actions"><button class="primary small" @click="saveDesc(s, props)">保存</button><button class="small" @click="s.editDesc := false">キャンセル</button></div>
          </div>
        </section>

        <template v-for="d in s.detail.mapC(_.toList)">

          <section v-if="d.subtasks.nonEmpty">
            <h3>サブタスク <span class="muted small">{{ d.task.doneSubtaskCount }}/{{ d.task.subtaskCount }}</span></h3>
            <div class="table-wrap">
              <table class="list"><tbody>
                <tr v-for="c in d.subtasks" :key="c.id" class="clickable" @click='Router.go("/tasks/" + c.key)'>
                  <td class="key">{{ c.key }}</td><td>{{ c.title }}</td>
                  <td><span :class='"badge cat-" + catOf(c.statusId)'>{{ nameOf(c.statusId) }}</span></td>
                  <td><Avatar :user="c.assignee" :size="20" /></td>
                </tr>
              </tbody></table>
            </div>
          </section>

          <section v-if="computed { d.links.nonEmpty || s.addingLink.v }">
            <h3>リンク</h3>
            <div v-for="l in d.links" :key="l.id" class="link-row">
              <span class="muted small kind">{{ LinkKind.label(l.kind) }}</span>
              <template v-if="l.target.isDefined">
                <RouterLink :to='"/tasks/" + l.target.get.key' class="key">{{ l.target.get.key }}</RouterLink>
                <span class="grow">{{ l.target.get.title }}</span>
                <span :class='"badge cat-" + l.target.get.statusCategory'>{{ l.target.get.statusName }}</span>
              </template>
              <template v-else><a :href='l.url.getOrElse("")' target="_blank" rel="noopener" class="grow">{{ l.title }}</a></template>
              <button v-if="canEdit(s)" class="ghost small" @click="removeLink(s, props, l)">×</button>
            </div>
            <form v-if="s.addingLink" class="row addlink" @submit.prevent="addLink(s, props)">
              <Combo :bind="Some(s.linkKind)" :options="linkKinds" />
              <input type="text" v-model="s.linkTarget" :placeholder='if (isWeb(s).v) "https://..." else "タスクID（例: DEMO-3）"' class="grow">
              <input v-if="isWeb(s)" type="text" v-model="s.linkTitle" placeholder="表示名（任意）">
              <button type="submit" class="primary small">追加</button>
            </form>
          </section>

          <section v-if="d.attachments.nonEmpty">
            <h3>添付ファイル</h3>
            <div class="atts">
              <div v-for="a in d.attachments" :key="a.id" class="att">
                <a :href="a.url" target="_blank" rel="noopener">
                  <img v-if='a.contentType.startsWith("image/")' :src="a.url" alt="" loading="lazy">
                  <span v-else class="file-icon">📄</span>
                </a>
                <div class="small att-name"><a :href="a.url" target="_blank" rel="noopener">{{ a.fileName }}</a></div>
                <div class="muted small">{{ Fmt.bytes(a.size) }} <button v-if="canEdit(s)" class="ghost small" @click="removeAttachment(s, props, a)">削除</button></div>
              </div>
            </div>
          </section>

          <section v-if="d.commits.nonEmpty">
            <h3>コミット</h3>
            <div v-for="c in d.commits" :key="c.sha" class="commit small">
              <a :href="Fmt.href(c.url)" target="_blank" rel="noopener" class="key">{{ c.sha.take(8) }}</a>
              <span class="grow">{{ c.message.linesIterator.nextOption().getOrElse("") }}</span>
              <span class="muted">{{ c.author }} · {{ c.repoName }} · {{ Fmt.ago(c.committedAt) }}</span>
            </div>
          </section>
        </template>

        <template v-for="i in s.ident.mapC(_.toList)">
          <nav class="tabs">
            <button :class='tabClass(s, "comments")' @click='s.tab := "comments"'>コメント</button>
            <button :class='tabClass(s, "worklogs")' @click='s.tab := "worklogs"'>作業時間</button>
            <button :class='tabClass(s, "history")' @click='s.tab := "history"'>履歴</button>
          </nav>
          <Comments v-if='s.tab.mapC(_ == "comments")' :taskId="i.id" :taskKey="i.key" :projectId="i.projectId" :canEdit="i.canEdit" :isAdmin="i.isAdmin" />
          <WorkPanel v-if='s.tab.mapC(_ == "worklogs")' :taskId="i.id" :canEdit="i.canEdit" tab="worklogs" :onChanged="_ => load(s, props)" />
          <WorkPanel v-if='s.tab.mapC(_ == "history")' :taskId="i.id" :canEdit="false" tab="history" />
        </template>
      </div>

      <aside class="side">
        <!-- ヘルプデスク: 問い合わせする"ユーザ"への共有（詳細を読み直しても作り直さない） -->
        <template v-for="i in s.ident.mapC(_.toList)">
          <SharePanel v-if="Store.helpdesk" :taskId="i.id" :projectId="i.projectId" :canEdit="i.canEdit" :onChanged="_ => load(s, props)" />
          <p v-if="Store.helpdesk" class="small"><RouterLink :to='"/help/t/" + i.key'>窓口での見え方を確認 →</RouterLink></p>
        </template>
        <template v-for="d in s.detail.mapC(_.toList)">
          <div class="field"><span>ステータス</span>
            <Combo :options="Opt.statuses(statuses.v)" :value="d.task.statusId.toString" :disabled="!canEdit(s).v"
              :onChange='v => patch(s, props, "statusId" -> Json.fromString(v))' />
          </div>
          <div class="two">
            <div class="field"><span>種類</span>
              <Combo :options="Opt.types" :value="d.task.taskType" :disabled="!canEdit(s).v" :onChange='v => patch(s, props, "taskType" -> Json.fromString(v))' />
            </div>
            <div class="field"><span>優先度</span>
              <Combo :options="Opt.priorities" :value="d.task.priority" :disabled="!canEdit(s).v" :onChange='v => patch(s, props, "priority" -> Json.fromString(v))' />
            </div>
          </div>
          <div class="field"><span>担当者</span>
            <Combo :options="Opt.users(s.audience.v)" :value='d.task.assignee.map(_.id.toString).getOrElse("")' clearLabel="未割り当て"
              :disabled="!canEdit(s).v" :onChange='v => patch(s, props, "assigneeId" -> opt(v))' />
          </div>
          <div class="field"><span>報告者</span>
            <Combo :options="Opt.users(s.audience.v)" :value='d.task.reporter.map(_.id.toString).getOrElse("")' clearLabel="なし"
              :disabled="!canEdit(s).v" :onChange='v => patch(s, props, "reporterId" -> opt(v))' />
          </div>
          <div v-if="Store.helpdesk.mapC(!_)" class="field"><span>スプリント</span>
            <Combo :options="sprintOpts.v" :value='d.task.sprintId.map(_.toString).getOrElse("")' clearLabel="バックログ"
              :disabled="!canEdit(s).v" :onChange='v => patch(s, props, "sprintId" -> opt(v))' />
          </div>
          <div class="dates">
            <label class="field"><span>開始日</span>
              <input type="date" :value='d.task.startDate.getOrElse("")' :disabled="canEdit(s).mapC(!_)" @change='patch(s, props, "startDate" -> opt(Dom.value(event)))'>
            </label>
            <label class="field"><span>期日</span>
              <input type="date" :class='overdue(s).mapC(o => if (o) "overdue" else "")' :value='d.task.dueDate.getOrElse("")' :disabled="canEdit(s).mapC(!_)"
                @change='patch(s, props, "dueDate" -> opt(Dom.value(event)))'>
            </label>
          </div>
          <div class="field"><span>作業時間</span>
            <div class="small">{{ spent(s) }}</div>
          </div>
          <div class="field"><span>ラベル</span>
            <div class="chips">
              <label v-for="l in labels" :key="l.id" class="lab">
                <input type="checkbox" :checked="d.task.labelIds.contains(l.id)" :disabled="canEdit(s).mapC(!_)" @change="toggleLabel(s, props, l, Dom.checked(event))">
                <span class="label-chip" :style='"background:" + l.color'>{{ l.name }}</span>
              </label>
            </div>
            <form v-if="canEdit(s)" class="row" @submit.prevent="addLabel(s, props)"><input type="text" v-model="s.newLabel" placeholder="新しいラベル" class="grow small"></form>
          </div>
          <div v-for="f in fields" :key="f.id" class="field">
            <span>{{ f.name }}</span>
            <FieldInput :field="f" :value="d.fields.get(f.id.toString)" :disabled="!canEdit(s).v" :onChange="v => setField(s, props, f, v)" />
          </div>
          <div class="field"><span>先行タスク（ガント）</span>
            <div v-for="p in d.predecessors" :key="p.id" class="row small">
              <RouterLink :to='"/tasks/" + p.key' class="key">{{ p.key }}</RouterLink><span class="grow ell">{{ p.title }}</span>
              <button v-if="canEdit(s)" class="ghost small" @click="removeDep(s, props, p)">×</button>
            </div>
            <form v-if="canEdit(s)" class="row" @submit.prevent="addDep(s, props)"><input type="text" v-model="s.depKey" placeholder="タスクID" class="grow small"></form>
            <div v-for="p in d.successors" :key="p.id" class="small muted">後続: <RouterLink :to='"/tasks/" + p.key'>{{ p.key }}</RouterLink> {{ p.title }}</div>
          </div>
          <div class="field"><span>ウォッチャー（{{ d.watchers.size }}）</span>
            <div v-for="w in d.watchers" :key="w.id" class="row small watcher">
              <Avatar :user="Some(w)" :size="20" /><span class="grow ell">{{ w.name }}</span>
              <button v-if="canEdit(s)" class="ghost small" @click="removeWatcher(s, props, w)" title="ウォッチャーから外す">×</button>
            </div>
            <Combo v-if="canEdit(s)" :options="Opt.users(watcherCandidates(s).v)" :small="true" :keep="false"
              placeholder="＋ ウォッチャーを追加（名前で検索）" :onChange="v => addWatcher(s, props, v)" />
            <button class="small" @click="watchToggle(s, props)">{{ if (d.watching) "👁 自分のウォッチをやめる" else "👁 自分もウォッチする" }}</button>
          </div>
          <div v-if="computed { s.rules.v.nonEmpty && canEdit(s).v }" class="field"><span>自動化</span>
            <button v-for="r in s.rules" :key="r.id" class="small" @click="runRule(s, r)">⚡ {{ r.name }}</button>
          </div>
          <div class="muted small meta">
            作成 {{ Fmt.dateTime(d.task.createdAt) }}<br>更新 {{ Fmt.dateTime(d.task.updatedAt) }}
            <template v-if="d.task.resolvedAt.isDefined"><br>完了 {{ d.task.resolvedAt.map(Fmt.dateTime).getOrElse("") }}</template>
          </div>
          <button v-if="canEdit(s)" class="danger small" @click="deleteTask(s, props)">タスクを削除</button>
        </template>
      </aside>
    </div>
    <template v-for="i in s.ident.mapC(_.toList)">
    </template>
  </div>
</template>

<style scoped>
.crumbs { margin-bottom: .4rem; color: var(--muted); }
.stale { background: rgba(217, 119, 6, .14); border-radius: 8px; padding: .5rem .8rem; margin-bottom: .6rem; }
.layout { display: grid; grid-template-columns: minmax(0, 1fr) 290px; gap: 2rem; align-items: start; }
@media (max-width: 900px) { .layout { grid-template-columns: 1fr; } }
.title { cursor: text; margin-bottom: .6rem; word-break: break-word; }
.title-input { font-size: 1.2rem; font-weight: 600; }
.toolbar { margin-bottom: .6rem; }
section { margin-bottom: 1.2rem; }
.desc { padding: .2rem 0; }
.actions { margin-top: .4rem; }
.link-row, .commit { display: flex; gap: .6rem; align-items: center; padding: .35rem 0; border-bottom: 1px solid var(--line); }
.kind { width: 8em; flex: none; }
.addlink { margin-top: .5rem; }
.atts { display: grid; grid-template-columns: repeat(auto-fill, minmax(140px, 1fr)); gap: .6rem; }
.att { border: 1px solid var(--line); border-radius: 8px; padding: .4rem; }
.att img { width: 100%; height: 90px; object-fit: cover; border-radius: 5px; display: block; }
.file-icon { display: flex; height: 90px; align-items: center; justify-content: center; font-size: 2rem; }
.att-name { word-break: break-all; margin-top: .2rem; }
.side { background: var(--side); border: 1px solid var(--line); border-radius: 10px; padding: 1rem; position: sticky; top: calc(var(--header) + 1rem); }
@media (max-width: 900px) { .side { position: static; } }
.side select, .side input[type="text"], .side input[type="date"], .side textarea { width: 100%; min-width: 0; }
.side .field { min-width: 0; }
.two { display: grid; grid-template-columns: 1fr 1fr; gap: 0 .5rem; }
.two .combo { min-width: 0; }
.dates { display: grid; grid-template-columns: repeat(auto-fit, minmax(118px, 1fr)); gap: 0 .5rem; }
.watcher { padding: .1rem 0; flex-wrap: nowrap; }
.tv.in-modal .layout { grid-template-columns: minmax(0, 1fr) 260px; gap: 1.2rem; }
.tv.in-modal .side { position: static; }
@media (max-width: 900px) { .tv.in-modal .layout { grid-template-columns: 1fr; } }
.lab { display: inline-flex; align-items: center; gap: .2rem; }
.ell { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.meta { margin: 1rem 0; }
.status-sel.cat-done { color: var(--ok); } .status-sel.cat-in_progress { color: var(--accent); } .status-sel.cat-waiting { color: var(--warn); }
</style>
