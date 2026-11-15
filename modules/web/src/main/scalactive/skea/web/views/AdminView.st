<script lang="scala">
import skea.web.*
import io.circe.syntax.*
import skea.shared.*
import skea.web.parts.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// 管理（システム管理者）: ユーザー・プロジェクト・監査ログ
case class State(tab: RV[String] = RV("users"),
                 users: RV[List[UserDto]] = RV(Nil),
                 q: RV[String] = RV(""),
                 projects: RV[List[ProjectDto]] = RV(Nil),
                 audit: RV[List[AuditDto]] = RV(Nil))

def initState() = State()

def load(s: State): Unit = {
  Api.get[List[UserDto]]("/api/admin/users" + Api.query("q" -> Some(s.q.v))).foreach(s.users := _)
  Api.projects(archived = true).foreach(s.projects := _)
  Api.get[List[AuditDto]]("/api/admin/audit?limit=200").foreach(s.audit := _)
}

def mounted(s: State): Unit = { pageTitle("管理"); load(s) }

def setUser(s: State, u: UserDto, admin: Option[Boolean], disabled: Option[Boolean]): Unit =
  Store.report(Api.patchUnit(s"/api/admin/users/${u.id}", UpdateUserReq(admin, disabled).asJson))(_ => load(s))

def archive(s: State, p: ProjectDto, on: Boolean): Unit =
  Store.report(Api.patchUnit(s"/api/projects/${p.id}", UpdateProjectReq(None, None, None, Some(on)).asJson)) { _ => load(s); Store.loadProjects() }

def remove(s: State, p: ProjectDto): Unit = {
  Dialog.prompt(s"プロジェクト「${p.name}」とすべてのタスクを完全に削除します。元に戻せません。\n確認のためキー（${p.key}）を入力してください。", "", "完全に削除する") { k =>
    if (k.trim.toUpperCase == p.key) Store.report(Api.delete(s"/api/projects/${p.id}")) { _ => load(s); Store.loadProjects() }
    else Store.toast("キーが一致しないため削除しませんでした", error = true)
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
              <td><Avatar :user="Some(UserBrief(u.id, u.name, u.avatarUrl))" :size="22" /> {{ u.name }}</td>
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
