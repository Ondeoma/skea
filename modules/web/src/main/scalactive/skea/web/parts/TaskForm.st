<script lang="scala">
import skea.web.*
import io.circe.Json
import skea.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

// タスクの作成フォーム
case class Props(projectId: UUID, parentId: Option[UUID] = None)

case class State(meta: RV[Option[ProjectMetaDto]] = RV(None),
                 audience: RV[List[UserBrief]] = RV(Nil),
                 title: RV[String] = RV(""),
                 statusId: RV[String] = RV(""),
                 assignee: RV[String] = RV(""),
                 sprint: RV[String] = RV(""),
                 start: RV[String] = RV(""),
                 due: RV[String] = RV(""),
                 labels: RV[List[String]] = RV(Nil),
                 fields: RV[Map[String, Json]] = RV(Map.empty),
                 editor: RV[Option[EditorHandle]] = RV(None),
                 more: RV[Boolean] = RV(false),
                 busy: RV[Boolean] = RV(false),
                 error: RV[String] = RV(""),
                 keepOpen: RV[Boolean] = RV(false),
                 templates: RV[List[TemplateDto]] = RV(Nil),
                 priority: RV[String] = RV(Priority.Medium),
                 taskType: RV[String] = RV(TaskType.Task),
                 template: RV[Option[TemplateDto]] = RV(None))

def initState() = State()

def mounted(s: State, props: Props): Unit = {
  Store.report(Api.meta(props.projectId)) { m =>
    s.meta := Some(m)
    s.statusId := m.statuses.headOption.map(_.id.toString).getOrElse("")
    // 開いているボードのスプリントに入れる（親があれば親と同じにする）
    s.sprint := m.sprints.find(_.state == "active").map(_.id.toString).getOrElse("")
  }
  Store.audience(props.projectId).foreach(s.audience := _)
  if (props.parentId.isEmpty) Api.get[List[TemplateDto]](s"/api/projects/${props.projectId}/templates").foreach(s.templates := _)
}

/** テンプレートの値を入れる（作成するとテンプレートのサブタスクも作る）. */
def applyTemplate(s: State, id: String): Unit = {
  val t = s.templates.v.find(_.id.toString == id)
  s.template := t
  t.foreach { x =>
    val today = Fmt.today()
    s.title := x.title.replace("{{date}}", today).replace("{{month}}", today.take(7))
    x.statusId.foreach(v => s.statusId := v.toString)
    s.assignee := x.assigneeId.map(_.toString).getOrElse("")
    s.labels := x.labelIds.map(_.toString)
    s.fields := x.fields
    s.due := x.dueInDays.map(d => Fmt.addDays(today, d)).getOrElse("")
    s.editor.v.foreach(_.setJSON(x.description.map(Editor.toJs).getOrElse(null)))
    if (x.labelIds.nonEmpty || x.fields.nonEmpty) s.more := true
  }
}

def close(): Unit = Store.createDialog := None

def setField(s: State, id: UUID, v: Json): Unit = s.fields := (if (v.isNull) s.fields.v - id.toString else s.fields.v.updated(id.toString, v))

def submit(s: State, props: Props): Unit = {
  s.error := ""
  if (s.title.v.trim.isEmpty) s.error := "タイトルを入力してください"
  else {
    s.busy := true
    val desc = s.editor.v.filterNot(_.isEmpty()).map(h => Editor.fromJs(h.getJSON()))
    val r = CreateTaskReq(
      title = s.title.v.trim,
      description = desc,
      statusId = Option(s.statusId.v).filter(_.nonEmpty).map(UUID.fromString),
      sprintId = Option(s.sprint.v).filter(_.nonEmpty).map(UUID.fromString),
      assigneeId = Option(s.assignee.v).filter(_.nonEmpty).map(UUID.fromString),
      parentId = props.parentId,
      startDate = Option(s.start.v).filter(_.nonEmpty),
      dueDate = Option(s.due.v).filter(_.nonEmpty),
      labelIds = s.labels.v.map(UUID.fromString),
      priority = Some(s.priority.v),
      taskType = Some(s.taskType.v),
      fields = s.fields.v.filter((_, v) => !v.isNull && !v.asString.contains("") && !v.asArray.exists(_.isEmpty))
    )
    Api.createTask(props.projectId, r).onComplete {
      case Success(t) =>
        s.busy := false
        // テンプレートのサブタスク
        s.template.v.toList.flatMap(_.subtasks).foldLeft(scala.concurrent.Future.successful(())) { (acc, st) =>
          acc.flatMap(_ => Api.createTask(props.projectId, CreateTaskReq(title = st, parentId = Some(t.id), sprintId = r.sprintId,
            assigneeId = r.assigneeId)).map(_ => ()))
        }.failed.foreach(Store.error)
        Store.toast(s"${t.key} を作成しました", Some(s"/tasks/${t.key}"))
        Store.changed()
        if (s.keepOpen.v) { s.title := ""; s.editor.v.foreach(_.clear()) } else close()
      case Failure(e) =>
        s.busy := false
        s.error := e.getMessage
    }
  }
}

def heading(props: Props): String = if (props.parentId.isDefined) "サブタスクを作成" else "タスクを作成"
def projectName(s: State) = s.meta.mapC(_.map(m => s"${m.project.key} ${m.project.name}").getOrElse(""))
def statusOpts(s: State) = s.meta.mapC(m => Opt.statuses(m.toList.flatMap(_.statuses)))
def sprintOpts(s: State) = s.meta.mapC(_.toList.flatMap(_.sprints).map(sp => Opt(sp.id.toString, sp.name, hint = if (sp.state == "active") "進行中" else "")))
def templateOpts(s: State) = s.templates.mapC(_.map(t => Opt(t.id.toString, t.name, hint = if (t.subtasks.nonEmpty) s"サブタスク ${t.subtasks.size}件" else "")))
def statuses(s: State) = s.meta.mapC(_.map(_.statuses).getOrElse(Nil))
def sprints(s: State) = s.meta.mapC(_.map(_.sprints).getOrElse(Nil))
def labels(s: State) = s.meta.mapC(_.map(_.labels).getOrElse(Nil))
def fields(s: State) = s.meta.mapC(_.map(_.fields).getOrElse(Nil))
</script>

<template>
  <form @submit.prevent="submit(s, props)">
    <h2>{{ heading(props) }} <span class="muted small">{{ projectName(s) }}</span></h2>
    <div v-if="s.templates.mapC(_.nonEmpty)" class="field"><span>テンプレート</span>
      <Combo :live="Some(templateOpts(s))" clearLabel="使わない" placeholder="テンプレートを選ぶ（名前で検索）" :onChange="v => applyTemplate(s, v)" />
    </div>
    <div class="field">
      <span>タイトル</span>
      <input type="text" v-model="s.title" placeholder="何をしますか？" autofocus>
    </div>
    <div class="grid">
      <div class="field"><span>種類</span><Combo :bind="Some(s.taskType)" :options="Opt.types" /></div>
      <div class="field"><span>優先度</span><Combo :bind="Some(s.priority)" :options="Opt.priorities" /></div>
      <div class="field"><span>ステータス</span><Combo :bind="Some(s.statusId)" :live="Some(statusOpts(s))" /></div>
      <div class="field"><span>担当者</span><Combo :bind="Some(s.assignee)" :live="Some(s.audience.mapC(Opt.users))" clearLabel="未割り当て" /></div>
      <div class="field"><span>スプリント</span><Combo :bind="Some(s.sprint)" :live="Some(sprintOpts(s))" clearLabel="バックログ" /></div>
      <label class="field"><span>期日</span><input type="date" v-model="s.due"></label>
    </div>
    <a href="#" class="small" @click.prevent="s.more := !s.more.v">{{ s.more.mapC(m => if (m) "▾ 詳細を閉じる" else "▸ 開始日・ラベル・カスタムフィールド") }}</a>
    <div v-show="s.more" class="more">
      <div class="grid">
        <label class="field"><span>開始日</span><input type="date" v-model="s.start"></label>
      </div>
      <div class="field">
        <span>ラベル</span>
        <div class="chips">
          <label v-for="l in labels(s)" :key="l.id" class="chip"><input type="checkbox" :value="l.id.toString" v-model="s.labels"> {{ l.name }}</label>
          <span v-if="labels(s).mapC(_.isEmpty)" class="muted small">ラベルはありません（タスクの画面で作れます）</span>
        </div>
      </div>
      <div v-for="f in fields(s)" :key="f.id" class="field">
        <span>{{ f.name }}{{ if (f.required) " *" else "" }}</span>
        <FieldInput :field="f" :value="None" :live="true" :onChange="v => setField(s, f.id, v)" />
      </div>
    </div>
    <div class="field">
      <span>説明</span>
      <RichEditor placeholder="詳しい内容（@でメンション）" :projectId="Some(props.projectId)" :onReady="h => s.editor := Some(h)" :onSubmit="_ => submit(s, props)" />
    </div>
    <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
    <div class="actions">
      <label class="small muted"><input type="checkbox" v-model="s.keepOpen"> 続けて作成</label>
      <span class="grow"></span>
      <button type="button" @click="close()">キャンセル</button>
      <button type="submit" class="primary" :disabled="s.busy">作成</button>
    </div>
  </form>
</template>

<style scoped>
.grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(140px, 1fr)); gap: 0 .7rem; }
.more { margin-top: .6rem; padding-top: .6rem; border-top: 1px dashed var(--line); }
.actions { align-items: center; }
</style>
