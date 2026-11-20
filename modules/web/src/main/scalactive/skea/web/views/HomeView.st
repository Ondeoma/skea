<script lang="scala">
import skea.web.*
import io.circe.syntax.*
import skea.shared.*
import skea.web.parts.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

// ホーム: 自分の担当・メンション・プロジェクト
case class State(mine: RV[List[SearchHitDto]] = RV(Nil),
                 mentions: RV[List[NotificationDto]] = RV(Nil),
                 creating: RV[Boolean] = RV(false),
                 key: RV[String] = RV(""),
                 name: RV[String] = RV(""),
                 error: RV[String] = RV(""))

def initState() = State()

def load(s: State): Unit = {
  Api.search("assignee:me is:open order:due,-updated", None, 30).foreach(r => s.mine := r.hits)
  Api.notifications(List(NotificationKind.Mention), unread = false).foreach(n => s.mentions := n.items.take(8))
}

def mounted(s: State): Unit = {
  pageTitle("ホーム")
  load(s)
}

def create(s: State): Unit = {
  s.error := ""
  Api.post[ProjectDto]("/api/projects", CreateProjectReq(s.key.v.trim.toUpperCase, s.name.v.trim, None).asJson).onComplete {
    case Success(p) =>
      s.creating := false
      Store.loadProjects()
      Router.go(s"/p/${p.key}/board")
    case Failure(e) => s.error := e.getMessage
  }
}

def hello = Store.me.mapC(_.map(m => s"${m.name} さん、こんにちは").getOrElse(""))
def noProjects = Store.projects.mapC(_.isEmpty)
def overdue(t: TaskDto): Boolean = Fmt.isOverdue(t.dueDate, t.resolvedAt.isDefined)
</script>

<template>
  <div class="page">
    <h1>{{ hello }}</h1>
    <div class="cols">
      <section>
        <h2>自分の担当（未完了）</h2>
        <div class="table-wrap">
          <table class="list">
            <tbody>
              <tr v-for="h in s.mine" :key="h.task.id" class="clickable" @click='Router.go("/tasks/" + h.task.key)'>
                <td class="key">{{ h.task.key }}</td>
                <td>{{ h.task.title }}</td>
                <td class="nowrap small" :class='if (overdue(h.task)) "overdue" else "muted"'>{{ h.task.dueDate.map(Fmt.day).getOrElse("") }}</td>
              </tr>
              <tr v-if="s.mine.mapC(_.isEmpty)"><td class="empty">担当している未完了のタスクはありません 🎉</td></tr>
            </tbody>
          </table>
        </div>
        <h2>最近のメンション</h2>
        <div class="table-wrap">
          <table class="list">
            <tbody>
              <tr v-for="n in s.mentions" :key="n.id" class="clickable" @click='Router.go(n.taskKey.fold("/notifications")(k => Store.taskLink(n.projectKey, k, n.commentId)))'>
                <td><Avatar :user="n.actor.map(person)" /></td>
                <td>{{ n.title }}<div class="muted small">{{ n.body }}</div></td>
                <td class="muted small nowrap">{{ Fmt.ago(n.at) }}</td>
              </tr>
              <tr v-if="s.mentions.mapC(_.isEmpty)"><td class="empty">メンションはありません</td></tr>
            </tbody>
          </table>
        </div>
      </section>
      <section>
        <div class="row"><h2>プロジェクト</h2><span class="grow"></span>
          <button v-if="Store.isAdmin" class="small" @click="s.creating := true">＋ プロジェクトを作成</button></div>
        <div class="projects">
          <RouterLink v-for="p in Store.projects" :key="p.id" :to='"/p/" + p.key + "/board"' class="pcard">
            <div class="key">{{ p.key }}</div>
            <div class="pname">{{ p.name }}</div>
            <div class="muted small">{{ Roles.label(p.role) }}</div>
          </RouterLink>
        </div>
        <p v-if="noProjects" class="muted">参加しているプロジェクトはありません。管理者にメンバーへの追加を依頼してください。</p>
        <p class="small"><RouterLink to="/worklogs">作業時間の一覧 →</RouterLink></p>
      </section>
    </div>
    <Modal v-if="s.creating" :onClose="_ => s.creating := false">
      <form @submit.prevent="create(s)">
          <h2>プロジェクトを作成</h2>
          <label class="field"><span>キー（英大文字で始まる2〜10文字。タスクIDの頭になります）</span><input type="text" v-model="s.key" placeholder="SKEA"></label>
          <label class="field"><span>名前</span><input type="text" v-model="s.name"></label>
          <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
          <div class="actions"><button type="button" @click="s.creating := false">キャンセル</button><button type="submit" class="primary">作成</button></div>
        </form>
    </Modal>
  </div>
</template>

<style scoped>
.cols { display: grid; grid-template-columns: 3fr 2fr; gap: 1.5rem; }
@media (max-width: 860px) { .cols { grid-template-columns: 1fr; } }
.projects { display: grid; grid-template-columns: repeat(auto-fill, minmax(160px, 1fr)); gap: .7rem; }
.pcard { display: block; border: 1px solid var(--line); border-radius: 10px; padding: .8rem; color: var(--fg); background: var(--card); }
.pcard:hover { border-color: var(--accent); text-decoration: none; }
.pname { font-weight: 600; margin: .2rem 0; }
</style>
