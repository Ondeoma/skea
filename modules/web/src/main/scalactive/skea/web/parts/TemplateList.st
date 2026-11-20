<script lang="scala">
import skea.web.*
import io.circe.Json
import io.circe.syntax.*
import skea.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// タスクのテンプレートの一覧と編集. personal なら自分用（どのプロジェクトでも使える）、そうでなければ開いているプロジェクトのもの.
// 自分用はプロジェクトに属する値（ステータス・担当者・ラベル・カスタムフィールド）を持たない.
case class Props(heading: String, note: String = "", personal: Boolean = false, onChanged: Unit => Unit = _ => ())

case class State(templates: RV[List[TemplateDto]] = RV(Nil),
                 editing: RV[Option[Option[TemplateDto]]] = RV(None),
                 name: RV[String] = RV(""),
                 title: RV[String] = RV(""),
                 desc: RV[Option[EditorHandle]] = RV(None),
                 status: RV[String] = RV(""),
                 assignee: RV[String] = RV(""),
                 priority: RV[String] = RV(""),
                 taskType: RV[String] = RV(""),
                 labels: RV[List[String]] = RV(Nil),
                 // カスタムフィールドの値. 画面の描き直しのきっかけにしない（入力のたびに入力欄が作り直されてフォーカスが外れるため）.
                 // 入力欄（FieldInput）は開いたときにここから最初の値を読み、あとは自身で値を持つ
                 fields: scala.collection.mutable.Map[String, Json] = scala.collection.mutable.Map.empty,
                 subtasks: RV[String] = RV(""),
                 due: RV[String] = RV(""))

def initState() = State()

def listUrl(props: Props): String = if (props.personal) "/api/me/templates" else s"/api/projects/${Store.pid}/templates"

def load(s: State, props: Props): Unit = Api.get[List[TemplateDto]](listUrl(props)).foreach(s.templates := _)

def mounted(s: State, props: Props): Unit = load(s, props)

def edit(s: State, t: Option[TemplateDto]): Unit = {
  s.name := t.map(_.name).getOrElse("")
  s.title := t.map(_.title).getOrElse("")
  s.status := t.flatMap(_.statusId).map(_.toString).getOrElse("")
  s.assignee := t.flatMap(_.assigneeId).map(_.toString).getOrElse("")
  s.priority := t.flatMap(_.priority).getOrElse("")
  s.taskType := t.flatMap(_.taskType).getOrElse("")
  s.labels := t.map(_.labelIds.map(_.toString)).getOrElse(Nil)
  s.fields.clear()
  s.fields ++= t.map(_.fields).getOrElse(Map.empty)
  s.subtasks := t.map(_.subtasks.mkString("\n")).getOrElse("")
  s.due := t.flatMap(_.dueInDays).map(_.toString).getOrElse("")
  s.editing := Some(t)
}

def opt(v: String): Option[UUID] = Option(v).filter(_.nonEmpty).map(UUID.fromString)

def save(s: State, props: Props): Unit = {
  val r = TemplateReq(s.name.v.trim, s.title.v.trim, s.desc.v.filterNot(_.isEmpty()).map(h => Editor.fromJs(h.getJSON())), opt(s.status.v),
    opt(s.assignee.v), s.labels.v.map(UUID.fromString), s.fields.toMap.filterNot(_._2.isNull), s.subtasks.v.split("\n").toList.map(_.trim).filter(_.nonEmpty),
    s.due.v.trim.toIntOption, Option(s.priority.v).filter(_.nonEmpty), Option(s.taskType.v).filter(_.nonEmpty))
  val f = s.editing.v.flatten match {
    case Some(t) => Api.putUnit(s"/api/templates/${t.id}", r.asJson)
    case None    => Api.post[TemplateDto](listUrl(props), r.asJson).map(_ => ())
  }
  Toasts.report(f) { _ => s.editing := None; load(s, props); props.onChanged(()) }
}

def remove(s: State, props: Props, t: TemplateDto): Unit = {
  val note = if (props.personal) "" else "（このテンプレートの繰り返しタスクも削除されます）"
  Dialog.confirm(s"テンプレート「${t.name}」を削除しますか？$note", "削除する") {
    Toasts.report(Api.delete(s"/api/templates/${t.id}")) { _ => load(s, props); props.onChanged(()) }
  }
}

def createFrom(t: TemplateDto): Unit =
  Toasts.report(Api.post[TaskDto](s"/api/templates/${t.id}/tasks")) { task =>
    Toasts.show(s"${task.key} を作成しました", Some(s"/tasks/${task.key}"))
    Store.changed()
  }

def setField(s: State, f: CustomFieldDto, v: Json): Unit = s.fields(f.id.toString) = v

def summary(t: TemplateDto): String =
  List(Some(t.title), t.taskType.map(TaskType.label), t.priority.map(p => "優先度 " + Priority.label(p)), t.dueInDays.map(d => s"期日 ${d}日後"),
    Option.when(t.subtasks.nonEmpty)(s"サブタスク ${t.subtasks.size}件")).flatten.mkString(" · ")

def canEdit(props: Props) = computed { props.personal || Store.canEdit.v }
def canCreate(props: Props) = computed { !props.personal && Store.canEdit.v }
def inProject(props: Props): Boolean = !props.personal
def statuses = Store.meta.mapC(_.map(_.statuses).getOrElse(Nil))
def members = Store.meta.mapC(_.map(_.members.map(_.user)).getOrElse(Nil))
def allLabels = Store.meta.mapC(_.map(_.labels).getOrElse(Nil))
def fieldDefs = Store.meta.mapC(_.map(_.fields).getOrElse(Nil))
def descContent(s: State): Option[Json] = s.editing.v.flatten.flatMap(_.description)
def editTitle(s: State) = s.editing.mapC(e => if (e.flatten.isDefined) "テンプレートを編集" else "テンプレートを作成")
def projectId(props: Props): Option[UUID] = if (props.personal) None else Some(Store.pid)
def empty(s: State) = s.templates.mapC(_.isEmpty)
</script>

<template>
  <div>
    <div class="row"><h2>{{ props.heading }}</h2><span class="grow"></span>
      <button v-if="canEdit(props)" class="primary small" @click="edit(s, None)">＋ テンプレートを作成</button></div>
    <p v-if="props.note.nonEmpty" class="muted small">{{ props.note }}</p>
    <div v-for="t in s.templates" :key="t.id" class="panel row trow">
      <div class="grow"><b>{{ t.name }}</b><div class="muted small">{{ summary(t) }}</div></div>
      <button v-if="canCreate(props)" class="small" @click="createFrom(t)">このテンプレートで作成</button>
      <button v-if="canEdit(props)" class="ghost small" @click="edit(s, Some(t))">編集</button>
      <button v-if="canEdit(props)" class="ghost small" @click="remove(s, props, t)">削除</button>
    </div>
    <p v-if="empty(s)" class="muted">テンプレートはありません</p>

    <Modal v-if="s.editing.mapC(_.isDefined)" :wide="true" :onClose="_ => s.editing := None">
      <form @submit.prevent="save(s, props)">
        <h2>{{ editTitle(s) }}</h2>
        <div class="grid2">
          <label class="field"><span>テンプレートの名前</span><input type="text" v-model="s.name" maxlength="100" placeholder="週次レポート"></label>
          <label class="field"><span>タスクのタイトル</span><input type="text" v-model="s.title" maxlength="500" placeholder="週次レポート &#123;&#123;date&#125;&#125;"></label>
        </div>
        <div class="field"><span>説明</span>
          <RichEditor :content="descContent(s)" :projectId="projectId(props)" placeholder="タスクの説明の初期値" :onReady="h => s.desc := Some(h)" />
        </div>
        <div class="grid2">
          <div class="field"><span>種類</span><Combo :bind="Some(s.taskType)" :options="Opts.types" clearLabel="既定" /></div>
          <div class="field"><span>優先度</span><Combo :bind="Some(s.priority)" :options="Opts.priorities" clearLabel="既定" /></div>
          <div v-if="inProject(props)" class="field"><span>ステータス</span><Combo :bind="Some(s.status)" :live="Some(statuses.mapC(Opts.statuses))" clearLabel="既定" /></div>
          <div v-if="inProject(props)" class="field"><span>担当者</span><Combo :bind="Some(s.assignee)" :live="Some(members.mapC(Opts.users))" clearLabel="未割り当て" /></div>
          <label class="field"><span>期日（作成日から何日後. 空ならなし）</span><input type="number" min="0" max="3650" v-model="s.due"></label>
        </div>
        <div v-if="inProject(props)" class="field"><span>ラベル</span>
          <div class="chips"><label v-for="l in allLabels" :key="l.id" class="chip"><input type="checkbox" :value="l.id.toString" v-model="s.labels"> {{ l.name }}</label></div>
        </div>
        <div v-if="inProject(props)">
          <div v-for="f in fieldDefs" :key="f.id" class="field"><span>{{ f.name }}</span>
            <FieldInput :field="f" :value="s.fields.get(f.id.toString)" :live="true" :onChange="v => setField(s, f, v)" />
          </div>
        </div>
        <label class="field"><span>一緒に作るサブタスク（1行に1つ）</span><textarea v-model="s.subtasks" placeholder="資料を集める&#10;下書きを書く&#10;レビューを受ける"></textarea></label>
        <div class="actions"><button type="button" @click="s.editing := None">キャンセル</button><button type="submit" class="primary">保存</button></div>
      </form>
    </Modal>
  </div>
</template>

<style scoped>
.trow { margin-bottom: .5rem; padding: .7rem 1rem; }
.grid2 { display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); gap: 0 .8rem; }
</style>
