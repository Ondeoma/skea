<script lang="scala">
import skea.web.*
import io.circe.syntax.*
import skea.shared.*
import skea.web.parts.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

// 管理（システム管理者）: ユーザー・エージェント専用アカウント・プロジェクト・監査ログ
case class State(tab: RV[String] = RV("users"),
                 users: RV[List[UserDto]] = RV(Nil),
                 q: RV[String] = RV(""),
                 projects: RV[List[ProjectDto]] = RV(Nil),
                 audit: RV[List[AuditDto]] = RV(Nil),
                 agents: RV[List[AgentAccountDto]] = RV(Nil),
                 agentName: RV[String] = RV(""),
                 keyFor: RV[Option[UUID]] = RV(None),
                 keyName: RV[String] = RV("skakapo"),
                 keyScopes: RV[List[String]] = RV(List("read", "write")),
                 keyDays: RV[String] = RV(""),
                 created: RV[Option[String]] = RV(None),
                 error: RV[String] = RV(""))

def initState() = State()

def load(s: State): Unit = {
  Api.get[List[UserDto]]("/api/admin/users" + Api.query("q" -> Some(s.q.v))).foreach(s.users := _)
  Api.projects(archived = true).foreach(s.projects := _)
  Api.get[List[AuditDto]]("/api/admin/audit?limit=200").foreach(s.audit := _)
  loadAgents(s)
}

// ---------------------------------------------------------------- エージェント専用アカウント

def loadAgents(s: State): Unit = Api.get[List[AgentAccountDto]]("/api/admin/agents").foreach(s.agents := _)

def createAgent(s: State): Unit =
  Toasts.report(Api.post[AgentAccountDto]("/api/admin/agents", CreateAgentReq(s.agentName.v).asJson)) { a =>
    s.agentName := ""; loadAgents(s); openKeyForm(s, a.id)
  }

def renameAgent(s: State, a: AgentAccountDto): Unit = Dialog.prompt("エージェント専用アカウントの名前", a.name, "変更") { n =>
  Toasts.report(Api.patchUnit(s"/api/admin/agents/${a.id}", UpdateAgentReq(Some(n), None).asJson))(_ => loadAgents(s))
}

def setAgentDisabled(s: State, a: AgentAccountDto, on: Boolean): Unit = {
  def run(): Unit = Toasts.report(Api.patchUnit(s"/api/admin/agents/${a.id}", UpdateAgentReq(None, Some(on)).asJson))(_ => loadAgents(s))
  if (on) Dialog.confirm(s"「${a.name}」を無効にしますか？このアカウントのキーはすべて使えなくなります（有効に戻すと再び使えます）。", "無効にする")(run())
  else run()
}

def openKeyForm(s: State, id: UUID): Unit = { s.keyFor := Some(id); s.created := None; s.error := "" }

def issueKey(s: State, a: AgentAccountDto): Unit = {
  s.error := ""
  Api.post[CreatedApiKeyDto](s"/api/admin/agents/${a.id}/keys", CreateApiKeyReq(s.keyName.v, s.keyScopes.v, None, s.keyDays.v.toIntOption).asJson).onComplete {
    case Success(r) => s.created := Some(r.key); s.keyFor := None; loadAgents(s)
    case Failure(e) => s.error := e.getMessage
  }
}

def revokeKey(s: State, a: AgentAccountDto, k: AgentKeyDto): Unit =
  Dialog.confirm(s"「${a.name}」のキー「${k.name}（${k.prefix}…）」を失効させますか？このキーを使っているエージェントは使えなくなります。", "失効させる") {
    Toasts.report(Api.delete(s"/api/admin/agents/${a.id}/keys/${k.id}"))(_ => loadAgents(s))
  }

def keyFormOpen(s: State, a: AgentAccountDto) = s.keyFor.mapC(_.contains(a.id))
def keyState(k: AgentKeyDto): String = if (k.revoked) "失効" else k.expiresAt.map(e => "期限 " + Fmt.dateTime(e).take(10)).getOrElse("無期限")

def mounted(s: State): Unit = { pageTitle("管理"); load(s) }

def setUser(s: State, u: UserDto, admin: Option[Boolean], disabled: Option[Boolean]): Unit =
  Toasts.report(Api.patchUnit(s"/api/admin/users/${u.id}", UpdateUserReq(admin, disabled).asJson))(_ => load(s))

def archive(s: State, p: ProjectDto, on: Boolean): Unit =
  Toasts.report(Api.patchUnit(s"/api/projects/${p.id}", UpdateProjectReq(None, None, None, Some(on)).asJson)) { _ => load(s); Store.loadProjects() }

def remove(s: State, p: ProjectDto): Unit = {
  Dialog.prompt(s"プロジェクト「${p.name}」とすべてのタスクを完全に削除します。元に戻せません。\n確認のためキー（${p.key}）を入力してください。", "", "完全に削除する") { k =>
    if (k.trim.toUpperCase == p.key) Toasts.report(Api.delete(s"/api/projects/${p.id}")) { _ => load(s); Store.loadProjects() }
    else Toasts.show("キーが一致しないため削除しませんでした", error = true)
  }
}

def tab(s: State, v: String) = s.tab.mapC(x => if (x == v) "active" else "")
def isTab(s: State, v: String) = s.tab.mapC(_ == v)
def me(u: UserDto): Boolean = Store.myId.contains(u.id)
</script>

<template>
  <div class="page">
    <h1>管理</h1>
    <nav class="tabs">
      <button :class='tab(s, "users")' @click='s.tab := "users"'>ユーザー</button>
      <button :class='tab(s, "agents")' @click='s.tab := "agents"'>エージェント</button>
      <button :class='tab(s, "projects")' @click='s.tab := "projects"'>プロジェクト</button>
      <button :class='tab(s, "audit")' @click='s.tab := "audit"'>監査ログ</button>
    </nav>
    <div v-if='isTab(s, "users")'>
      <form class="row" @submit.prevent="load(s)"><input type="search" v-model="s.q" placeholder="名前・メールで絞り込み"><button type="submit">検索</button></form>
      <p class="muted small">ユーザーは初めてログインしたときに作られます。プロジェクトごとの権限は各プロジェクトの「設定 → メンバー」で設定します。
        IdP のグループ（ADMIN_GROUPS）に入っている人は、ここでの設定によらず管理者です。</p>
      <div class="table-wrap">
        <table class="list">
          <thead><tr><th>名前</th><th>メール</th><th>グループ</th><th>最終ログイン</th><th>管理者</th><th>無効</th></tr></thead>
          <tbody>
            <tr v-for="u in s.users" :key="u.id">
              <td><Avatar :user="Some(Person(u.id.toString, u.name, u.avatarUrl))" :size="22" /> {{ u.name }}</td>
              <td class="small">{{ u.email.getOrElse("") }}</td>
              <td class="small">{{ u.groups.mkString(", ") }}</td>
              <td class="small">{{ u.lastLoginAt.map(Fmt.ago).getOrElse("") }}</td>
              <td><input type="checkbox" :checked="u.isAdmin" :disabled="me(u)" @change="setUser(s, u, Some(Dom.checked(event)), None)"></td>
              <td><input type="checkbox" :checked="u.disabled" :disabled="me(u)" @change="setUser(s, u, None, Some(Dom.checked(event)))"></td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>
    <div v-if='isTab(s, "agents")'>
      <p class="muted small">エージェント専用アカウントは、外部の AI エージェント（skakapo など）が skea の MCP（<code>/mcp</code>）・REST API を使うための、ログインしないアカウントです。
        エージェントはこのアカウント自身の権限で動きます。見せたい・書き込ませたいプロジェクトの「設定 → メンバー」で、ふつうのユーザーと同じようにこのアカウントをメンバーに追加してください（管理者にはなれません）。
        エージェントが行った作成・変更・コメントは、このアカウントの名前で記録されます。</p>
      <form class="row" @submit.prevent="createAgent(s)">
        <input type="text" v-model="s.agentName" maxlength="60" placeholder="名前（例: skakapo）" class="grow">
        <button type="submit" class="primary">アカウントを作る</button>
      </form>
      <div v-if="s.created.mapC(_.isDefined)" class="created">
        <p><b>キーを発行しました。</b>この画面を閉じると二度と表示されません。skakapo の管理画面の「サービス」→ このサービス →「エージェント専用アカウント」に貼り付けてください。</p>
        <div class="row"><code class="grow keytext">{{ s.created.mapC(_.getOrElse("")) }}</code><button class="small" @click="Dom.copy(s.created.v.getOrElse(&quot;&quot;))">コピー</button></div>
        <button class="small" @click="s.created := None">閉じる</button>
      </div>
      <section v-for="a in s.agents" :key="a.id" class="panel agent">
        <div class="row">
          <b>{{ a.name }}</b>
          <span v-if="a.disabled" class="badge">無効</span>
          <span class="muted small">作成 {{ Fmt.dateTime(a.createdAt).take(10) }}</span>
          <span class="grow"></span>
          <button class="small" @click="renameAgent(s, a)">名前を変える</button>
          <button class="small" @click="setAgentDisabled(s, a, !a.disabled)">{{ if (a.disabled) "有効にする" else "無効にする" }}</button>
          <button v-if="!a.disabled" class="small primary" @click="openKeyForm(s, a.id)">キーを発行</button>
        </div>
        <form v-if="keyFormOpen(s, a)" class="keyform" @submit.prevent="issueKey(s, a)">
          <div class="row">
            <input type="text" v-model="s.keyName" placeholder="用途（例: skakapo）" class="grow">
            <Combo :bind="Some(s.keyDays)" :options='Opt.of(List("" -> "無期限", "30" -> "30日", "90" -> "90日", "365" -> "1年"))' />
          </div>
          <div class="row">
            <span class="small muted">スコープ:</span>
            <label class="small"><input type="checkbox" value="read" v-model="s.keyScopes"> 参照（read）</label>
            <label class="small"><input type="checkbox" value="write" v-model="s.keyScopes"> 作成・更新・削除（write）</label>
          </div>
          <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
          <button type="submit" class="primary small">発行</button>
          <button type="button" class="small" @click="s.keyFor := None">やめる</button>
        </form>
        <div class="table-wrap">
          <table class="list">
            <thead><tr><th>用途</th><th>キー</th><th>スコープ</th><th>状態</th><th>最終使用</th><th></th></tr></thead>
            <tbody>
              <tr v-for="k in a.keys" :key="k.id" :class='if (k.revoked) "muted" else ""'>
                <td>{{ k.name }}</td><td><code>{{ k.prefix }}…</code></td><td>{{ k.scopes.mkString(", ") }}</td>
                <td class="small">{{ keyState(k) }}</td>
                <td class="small">{{ k.lastUsedAt.map(Fmt.ago).getOrElse("未使用") }}</td>
                <td><button v-if="!k.revoked" class="danger small" @click="revokeKey(s, a, k)">失効</button></td>
              </tr>
              <tr v-if="a.keys.isEmpty"><td colspan="6" class="muted">発行済みのキーはありません</td></tr>
            </tbody>
          </table>
        </div>
      </section>
      <p v-if="s.agents.mapC(_.isEmpty)" class="muted">エージェント専用アカウントはまだありません。</p>
    </div>
    <div v-if='isTab(s, "projects")'>
      <p class="muted small">プロジェクトの作成はホームの「＋ プロジェクトを作成」から行えます。</p>
      <div class="table-wrap">
        <table class="list">
          <thead><tr><th>キー</th><th>名前</th><th>リーダー</th><th>作成</th><th></th></tr></thead>
          <tbody>
            <tr v-for="p in s.projects" :key="p.id">
              <td class="key">{{ p.key }}</td>
              <td><RouterLink :to='"/p/" + p.key + "/settings/general"'>{{ p.name }}</RouterLink> <span v-if="p.archived" class="badge">アーカイブ</span></td>
              <td class="small">{{ p.lead.map(_.name).getOrElse("") }}</td>
              <td class="small">{{ Fmt.dateTime(p.createdAt).take(10) }}</td>
              <td class="nowrap">
                <button class="small" @click="archive(s, p, !p.archived)">{{ if (p.archived) "アーカイブを解除" else "アーカイブ" }}</button>
                <button class="danger small" @click="remove(s, p)">削除</button>
              </td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>
    <div v-if='isTab(s, "audit")' class="table-wrap">
      <table class="list">
        <thead><tr><th>日時</th><th>操作者</th><th>経路</th><th>操作</th><th>対象</th><th>詳細</th></tr></thead>
        <tbody>
          <tr v-for="a in s.audit" :key="a.id">
            <td class="small nowrap">{{ Fmt.dateTime(a.at) }}</td>
            <td class="small">{{ a.actor.map(_.name).getOrElse("") }}</td>
            <td class="small">{{ a.via }}</td>
            <td><code class="small">{{ a.action }}</code></td>
            <td class="small key">{{ a.target.getOrElse("").take(8) }}</td>
            <td class="small muted">{{ a.detail.noSpaces.take(120) }}</td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
</template>

<style scoped>
.agent { margin-top: 1rem; }
.keyform { margin: .6rem 0; }
.created { border: 1px solid var(--ok); border-radius: 8px; padding: .8rem 1rem; margin: 1rem 0; background: color-mix(in srgb, var(--ok) 8%, transparent); }
.keytext { background: var(--side); padding: .3rem .5rem; border-radius: 5px; word-break: break-all; font-size: .8rem; }
</style>
