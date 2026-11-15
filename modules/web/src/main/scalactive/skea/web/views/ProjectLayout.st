<script lang="scala">
import skea.web.*
import io.github.ondeoma.scalactive.components.routes.RouterComponent
import skea.web.settings.ProjectSettings

// プロジェクトの画面の枠（ボード・バックログ・一覧・ガント・活動・設定）
case class Props(key: String)

case class State(error: RV[String] = RV(""))

def initState() = State()

def mounted(s: State, props: Props): Unit = {
  Store.meta := Store.meta.v.filter(_.project.key == props.key)
  Store.openProject(props.key).failed.foreach(e => s.error := e.getMessage)(scala.scalajs.concurrent.JSExecutionContext.Implicits.queue)
}

val routes: PathPatternRoutes = List(
  ("board", _ => BoardView()),
  ("backlog", _ => BacklogView()),
  ("list", _ => ListView()),
  ("gantt", _ => GanttView()),
  ("activity", _ => ActivityView()),
  ("settings/*", _ => ProjectSettings()),
)

def loaded(props: Props) = Store.meta.mapC(_.exists(_.project.key == props.key))
def name = Store.meta.mapC(_.map(_.project.name).getOrElse(""))
def create(): Unit = Store.meta.v.foreach(m => Store.createDialog := Some((m.project.id, None)))
def base(props: Props): String = "/p/" + props.key
</script>

<template>
  <div class="sub">
    <div class="title"><span class="key">{{ props.key }}</span> <b>{{ name }}</b></div>
    <nav class="tabs subtabs">
      <RouterLink :to='base(props) + "/board"'>ボード</RouterLink>
      <RouterLink :to='base(props) + "/backlog"'>バックログ</RouterLink>
      <RouterLink :to='base(props) + "/list"'>一覧</RouterLink>
      <RouterLink :to='base(props) + "/gantt"'>ガント</RouterLink>
      <RouterLink :to='base(props) + "/activity"'>活動</RouterLink>
      <RouterLink :to='base(props) + "/settings/general"'>設定</RouterLink>
    </nav>
    <button v-if="Store.canEdit" class="primary small create" @click="create()">＋ タスク</button>
  </div>
  <p v-if="s.error.mapC(_.nonEmpty)" class="page error">{{ s.error }}</p>
  <RouterComponent v-if="loaded(props)" :routes="routes" :prefix="base(props)" />
</template>

<style scoped>
.sub { display: flex; align-items: center; gap: 1rem; padding: 0 1rem; border-bottom: 1px solid var(--line); background: var(--side); position: sticky;
  top: var(--header); z-index: 50; }
.title { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 30%; }
.subtabs { border-bottom: none; margin: 0; flex: 1; }
.create { flex: none; }
@media (max-width: 720px) { .title { display: none; } .sub { padding: 0 .5rem; gap: .4rem; } }
</style>
