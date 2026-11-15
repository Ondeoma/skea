<script lang="scala">
import skea.web.*
import skea.web.parts.*
import io.circe.Json
import io.circe.syntax.*
import skea.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// タスクのテンプレートと、繰り返しタスク（テンプレートから毎日・毎週・毎月作る）
case class State(templates: RV[List[TemplateDto]] = RV(Nil),
                 recurring: RV[List[RecurringDto]] = RV(Nil),
                 editing: RV[Option[Option[TemplateDto]]] = RV(None),
                 name: RV[String] = RV(""),
                 title: RV[String] = RV(""),
                 desc: RV[Option[EditorHandle]] = RV(None),
                 status: RV[String] = RV(""),
                 assignee: RV[String] = RV(""),
                 labels: RV[List[String]] = RV(Nil),
                 fields: RV[Map[String, Json]] = RV(Map.empty),
                 subtasks: RV[String] = RV(""),
                 due: RV[String] = RV(""),
                 rEditing: RV[Option[Option[RecurringDto]]] = RV(None),
                 rTemplate: RV[String] = RV(""),
                 rFreq: RV[String] = RV(Frequency.Weekly),
                 rWeekday: RV[String] = RV("1"),
                 rMonthday: RV[String] = RV("1"),
                 rSprint: RV[Boolean] = RV(false),
                 rEnabled: RV[Boolean] = RV(true),
                 rStart: RV[String] = RV(""))

def initState() = State()

def load(s: State): Unit = {
  Api.get[List[TemplateDto]](s"/api/projects/${Store.pid}/templates").foreach(s.templates := _)
  Api.get[List[RecurringDto]](s"/api/projects/${Store.pid}/recurring").foreach(s.recurring := _)
}

def mounted(s: State): Unit = load(s)

// ---------------------------------------------------------------- テンプレート

def edit(s: State, t: Option[TemplateDto]): Unit = {
  s.name := t.map(_.name).getOrElse("")
  s.title := t.map(_.title).getOrElse("")
  s.status := t.flatMap(_.statusId).map(_.toString).getOrElse("")
  s.assignee := t.flatMap(_.assigneeId).map(_.toString).getOrElse("")
  s.labels := t.map(_.labelIds.map(_.toString)).getOrElse(Nil)
  s.fields := t.map(_.fields).getOrElse(Map.empty)
  s.subtasks := t.map(_.subtasks.mkString("\n")).getOrElse("")
  s.due := t.flatMap(_.dueInDays).map(_.toString).getOrElse("")
  s.editing := Some(t)
}

def opt(v: String): Option[UUID] = Option(v).filter(_.nonEmpty).map(UUID.fromString)

def save(s: State): Unit = {
  val r = TemplateReq(s.name.v.trim, s.title.v.trim, s.desc.v.filterNot(_.isEmpty()).map(h => Editor.fromJs(h.getJSON())), opt(s.status.v),
    opt(s.assignee.v), s.labels.v.map(UUID.fromString), s.fields.v.filterNot(_._2.isNull), s.subtasks.v.split("\n").toList.map(_.trim).filter(_.nonEmpty),
    s.due.v.trim.toIntOption)
  val f = s.editing.v.flatten match {
    case Some(t) => Api.putUnit(s"/api/templates/${t.id}", r.asJson)
    case None    => Api.post[TemplateDto](s"/api/projects/${Store.pid}/templates", r.asJson).map(_ => ())
  }
  Store.report(f) { _ => s.editing := None; load(s) }
}

def remove(s: State, t: TemplateDto): Unit =
  Dialog.confirm(s"テンプレート「${t.name}」を削除しますか？（このテンプレートの繰り返しタスクも削除されます）", "削除する") {
    Store.report(Api.delete(s"/api/templates/${t.id}"))(_ => load(s))
  }

def createFrom(t: TemplateDto): Unit =
  Store.report(Api.post[TaskDto](s"/api/templates/${t.id}/tasks")) { task =>
    Store.toast(s"${task.key} を作成しました", Some(s"/tasks/${task.key}"))
    Store.changed()
  }

def setField(s: State, f: CustomFieldDto, v: Json): Unit = s.fields := s.fields.v.updated(f.id.toString, v)

def summary(t: TemplateDto): String =
  List(Some(t.title), t.dueInDays.map(d => s"期日 ${d}日後"), Option.when(t.subtasks.nonEmpty)(s"サブタスク ${t.subtasks.size}件")).flatten.mkString(" · ")

def statuses = Store.meta.mapC(_.map(_.statuses).getOrElse(Nil))
def members = Store.meta.mapC(_.map(_.members.map(_.user)).getOrElse(Nil))
def allLabels = Store.meta.mapC(_.map(_.labels).getOrElse(Nil))
def fieldDefs = Store.meta.mapC(_.map(_.fields).getOrElse(Nil))
def descContent(s: State): Option[Json] = s.editing.v.flatten.flatMap(_.description)
def editTitle(s: State) = s.editing.mapC(e => if (e.flatten.isDefined) "テンプレートを編集" else "テンプレートを作成")

// ---------------------------------------------------------------- 繰り返しタスク

def editR(s: State, r: Option[RecurringDto]): Unit = {
  s.rTemplate := r.map(_.templateId.toString).orElse(s.templates.v.headOption.map(_.id.toString)).getOrElse("")
  s.rFreq := r.map(_.frequency).getOrElse(Frequency.Weekly)
  s.rWeekday := r.flatMap(_.weekday).getOrElse(1).toString
  s.rMonthday := r.flatMap(_.monthday).getOrElse(1).toString
  s.rSprint := r.exists(_.activeSprint)
  s.rEnabled := r.forall(_.enabled)
  s.rStart := ""
  s.rEditing := Some(r)
}

def saveR(s: State): Unit = {
  val req = RecurringReq(UUID.fromString(s.rTemplate.v), s.rFreq.v, Option.when(s.rFreq.v == Frequency.Weekly)(s.rWeekday.v.toInt),
    Option.when(s.rFreq.v == Frequency.Monthly)(s.rMonthday.v.toInt), s.rSprint.v, s.rEnabled.v, Option(s.rStart.v).filter(_.nonEmpty))
  val f = s.rEditing.v.flatten match {
    case Some(r) => Api.putUnit(s"/api/recurring/${r.id}", req.asJson)
    case None    => Api.post[Json](s"/api/projects/${Store.pid}/recurring", req.asJson).map(_ => ())
  }
  Store.report(f) { _ => s.rEditing := None; load(s) }
}

def removeR(s: State, r: RecurringDto): Unit =
  Dialog.confirm(s"「${r.templateName}」の繰り返しをやめますか？", "やめる") { Store.report(Api.delete(s"/api/recurring/${r.id}"))(_ => load(s)) }

val weekdayOpts: List[Opt] = (1 to 7).toList.map(i => Opt(i.toString, Frequency.weekdays(i - 1) + "曜日"))
val monthdayOpts: List[Opt] = (1 to 31).toList.map(i => Opt(i.toString, s"${i}日"))
def rule(r: RecurringDto): String = r.frequency match {
  case Frequency.Weekly  => s"毎週 ${r.weekday.map(d => Frequency.weekdays(d - 1)).getOrElse("")}曜日"
  case Frequency.Monthly => s"毎月 ${r.monthday.getOrElse(1)}日"
  case _                 => "毎日"
}
def isFreq(s: State, f: String) = s.rFreq.mapC(_ == f)
def rTitle(s: State) = s.rEditing.mapC(e => if (e.flatten.isDefined) "繰り返しを編集" else "繰り返しを追加")
def noTemplates(s: State) = s.templates.mapC(_.isEmpty)
</script>

<template>
  <section>
    <div class="row"><h2>テンプレート</h2><span class="grow"></span><button v-if="Store.canEdit" class="primary small" @click="edit(s, None)">＋ テンプレートを作成</button></div>
    <p class="muted small">よく作るタスクの初期値（タイトル・説明・担当者・ラベル・期日など）と、一緒に作るサブタスクを登録します。
      タスクの作成画面で選べるほか、下の「繰り返し」で決まった日に自動で作れます。タイトルの <code>&#123;&#123;date&#125;&#125;</code>・<code>&#123;&#123;month&#125;&#125;</code> は作成日（2026-11-15・2026-11）になります。</p>
    <div v-for="t in s.templates" :key="t.id" class="panel row trow">
      <div class="grow"><b>{{ t.name }}</b><div class="muted small">{{ summary(t) }}</div></div>
      <button v-if="Store.canEdit" class="small" @click="createFrom(t)">このテンプレートで作成</button>
      <button v-if="Store.canEdit" class="ghost small" @click="edit(s, Some(t))">編集</button>
      <button v-if="Store.canEdit" class="ghost small" @click="remove(s, t)">削除</button>
    </div>
    <p v-if="noTemplates(s)" class="muted">テンプレートはありません</p>

    <div class="row"><h2>繰り返し</h2><span class="grow"></span>
      <button v-if="Store.canEdit" class="primary small" :disabled="noTemplates(s)" @click="editR(s, None)">＋ 繰り返しを追加</button></div>
    <p class="muted small">テンプレートから、毎日・毎週・毎月の決まった日にタスクを作ります（作成者として作ります）。</p>
    <div class="table-wrap">
      <table class="list"><thead><tr><th>テンプレート</th><th>頻度</th><th>次に作る日</th><th>前回</th><th></th></tr></thead>
        <tbody>
          <tr v-for="r in s.recurring" :key="r.id">
            <td>{{ r.templateName }} <span v-if="!r.enabled" class="badge">停止中</span> <span v-if="r.activeSprint" class="badge">進行中のスプリントへ</span></td>
            <td>{{ rule(r) }}</td>
            <td>{{ Fmt.day(r.nextRun) }}</td>
            <td class="small"><RouterLink v-if="r.lastTaskKey.isDefined" :to='"/tasks/" + r.lastTaskKey.getOrElse("")'>{{ r.lastTaskKey.getOrElse("") }}</RouterLink>
              <span class="muted">{{ r.lastRunAt.map(Fmt.ago).getOrElse("") }}</span></td>
            <td class="nowrap"><button v-if="Store.canEdit" class="ghost small" @click="editR(s, Some(r))">編集</button>
              <button v-if="Store.canEdit" class="ghost small" @click="removeR(s, r)">削除</button></td>
          </tr>
        </tbody>
      </table>
    </div>

    <Modal v-if="s.editing.mapC(_.isDefined)" :wide="true" :onClose="_ => s.editing := None">
      <form @submit.prevent="save(s)">
        <h2>{{ editTitle(s) }}</h2>
        <div class="grid2">
          <label class="field"><span>テンプレートの名前</span><input type="text" v-model="s.name" placeholder="週次レポート"></label>
          <label class="field"><span>タスクのタイトル</span><input type="text" v-model="s.title" placeholder="週次レポート &#123;&#123;date&#125;&#125;"></label>
        </div>
        <div class="field"><span>説明</span>
          <RichEditor :content="descContent(s)" :projectId="Some(Store.pid)" placeholder="タスクの説明の初期値" :onReady="h => s.desc := Some(h)" />
        </div>
        <div class="grid2">
          <div class="field"><span>ステータス</span><Combo :bind="Some(s.status)" :live="Some(statuses.mapC(Opt.statuses))" clearLabel="既定" /></div>
          <div class="field"><span>担当者</span><Combo :bind="Some(s.assignee)" :live="Some(members.mapC(Opt.users))" clearLabel="未割り当て" /></div>
          <label class="field"><span>期日（作成日から何日後. 空ならなし）</span><input type="number" min="0" v-model="s.due"></label>
        </div>
        <div class="field"><span>ラベル</span>
          <div class="chips"><label v-for="l in allLabels" :key="l.id" class="chip"><input type="checkbox" :value="l.id.toString" v-model="s.labels"> {{ l.name }}</label></div>
        </div>
        <div v-for="f in fieldDefs" :key="f.id" class="field"><span>{{ f.name }}</span>
          <FieldInput :field="f" :value="s.fields.v.get(f.id.toString)" :live="true" :onChange="v => setField(s, f, v)" />
        </div>
        <label class="field"><span>一緒に作るサブタスク（1行に1つ）</span><textarea v-model="s.subtasks" placeholder="資料を集める&#10;下書きを書く&#10;レビューを受ける"></textarea></label>
        <div class="actions"><button type="button" @click="s.editing := None">キャンセル</button><button type="submit" class="primary">保存</button></div>
      </form>
    </Modal>

    <Modal v-if="s.rEditing.mapC(_.isDefined)" :onClose="_ => s.rEditing := None">
      <form @submit.prevent="saveR(s)">
        <h2>{{ rTitle(s) }}</h2>
        <div class="field"><span>テンプレート</span><Combo :bind="Some(s.rTemplate)" :live="Some(s.templates.mapC(_.map(t => Opt(t.id.toString, t.name))))" /></div>
        <div class="field"><span>頻度</span>
          <div class="chips"><label v-for="f in Frequency.all" class="chip"><input type="radio" name="freq" :value="f" v-model="s.rFreq"> {{ Frequency.label(f) }}</label></div>
        </div>
        <div v-if='isFreq(s, Frequency.Weekly)' class="field"><span>曜日</span><Combo :bind="Some(s.rWeekday)" :options="weekdayOpts" /></div>
        <div v-if='isFreq(s, Frequency.Monthly)' class="field"><span>日（月末を超えるときは月末）</span><Combo :bind="Some(s.rMonthday)" :options="monthdayOpts" /></div>
        <label class="field"><span>開始日（空なら今日より後の最初の日）</span><input type="date" v-model="s.rStart"></label>
        <label class="small"><input type="checkbox" v-model="s.rSprint"> 進行中のスプリントに入れる</label><br>
        <label class="small"><input type="checkbox" v-model="s.rEnabled"> 有効</label>
        <div class="actions"><button type="button" @click="s.rEditing := None">キャンセル</button><button type="submit" class="primary">保存</button></div>
      </form>
    </Modal>
  </section>
</template>

<style scoped>
.trow { margin-bottom: .5rem; padding: .7rem 1rem; }
.grid2 { display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); gap: 0 .8rem; }
</style>
