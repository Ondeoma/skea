<script lang="scala">
import skea.web.*
import io.circe.syntax.*
import skea.shared.*
import skea.web.parts.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// メンバーと権限（ユーザーごと・IdP のグループごと）
case class State(members: RV[List[MemberDto]] = RV(Nil),
                 groups: RV[List[GroupRoleDto]] = RV(Nil),
                 q: RV[String] = RV(""),
                 candidates: RV[List[UserBrief]] = RV(Nil),
                 role: RV[String] = RV(Roles.Edit),
                 group: RV[String] = RV(""),
                 groupRole: RV[String] = RV(Roles.View))

def initState() = State()


def load(s: State): Unit = {
  Api.get[List[MemberDto]](s"/api/projects/${Store.pid}/members").foreach(s.members := _)
  Api.get[List[GroupRoleDto]](s"/api/projects/${Store.pid}/groups").foreach(s.groups := _)
}

def mounted(s: State): Unit = load(s)

def search(s: State): Unit = if (s.q.v.trim.nonEmpty) Api.users(s.q.v.trim).foreach(s.candidates := _)

def add(s: State, u: UserBrief): Unit =
  Store.report(Api.putUnit(s"/api/projects/${Store.pid}/members", SetMemberReq(u.id, s.role.v).asJson)) { _ =>
    s.candidates := Nil; s.q := ""; load(s); Store.reloadMeta()
  }

def setRole(s: State, m: MemberDto, role: String): Unit =
  Store.report(Api.putUnit(s"/api/projects/${Store.pid}/members", SetMemberReq(m.user.id, role).asJson)) { _ => load(s); Store.reloadMeta() }

def remove(s: State, m: MemberDto): Unit =
  Dialog.confirm(s"${m.user.name} さんをメンバーから外しますか？") {
    Store.report(Api.delete(s"/api/projects/${Store.pid}/members/${m.user.id}")) { _ => load(s); Store.reloadMeta() }
  }

def addGroup(s: State): Unit = if (s.group.v.trim.nonEmpty)
  Store.report(Api.putUnit(s"/api/projects/${Store.pid}/groups", SetGroupRoleReq(s.group.v.trim, s.groupRole.v).asJson)) { _ => s.group := ""; load(s) }

def removeGroup(s: State, g: GroupRoleDto): Unit = Store.report(Api.delete(s"/api/projects/${Store.pid}/groups/${g.groupId}"))(_ => load(s))

/** ヘルプデスクのプロジェクトでは、問い合わせする"ユーザ"も選べる. */
def roleOpts: List[Opt] =
  (if (Store.meta.v.exists(_.project.helpdesk)) Roles.withRequester else Roles.all).map(r => Opt(r, Roles.label(r)))
</script>

<template>
  <section>
    <h2>メンバーと権限</h2>
    <p class="muted small">閲覧: 見るだけ ／ 編集: タスク・コメント・スプリントの操作 ／ 管理: このプロジェクトの設定・メンバー。システム管理者はすべてのプロジェクトを管理できます。</p>
    <p v-if="Store.helpdesk" class="muted small">ヘルプデスク: 「ユーザ（問い合わせ）」は窓口の簡素な画面だけを使い、自分の問い合わせと共有されたタスクだけが見えます。閲覧以上の人が回答するエージェントです。</p>
    <form v-if="Store.isProjectAdmin" class="row" @submit.prevent="search(s)">
      <input type="search" v-model="s.q" placeholder="追加するユーザーを名前・メールで検索" class="grow">
      <Combo :bind="Some(s.role)" :options="roleOpts" />
      <button type="submit">検索</button>
    </form>
    <div v-if="s.candidates.mapC(_.nonEmpty)" class="cands">
      <button v-for="u in s.candidates" :key="u.id" class="small" @click="add(s, u)"><Avatar :user="Some(u)" :size="18" /> {{ u.name }} を追加</button>
    </div>
    <div class="table-wrap">
      <table class="list">
        <tbody>
          <tr v-for="m in s.members" :key="m.user.id">
            <td><Avatar :user="Some(m.user)" :size="24" /> {{ m.user.name }}</td>
            <td>
              <Combo :options="roleOpts" :value="m.role" :small="true" :disabled="!Store.isProjectAdmin.v" :onChange="v => setRole(s, m, v)" />
            </td>
            <td><button v-if="Store.isProjectAdmin" class="ghost small" @click="remove(s, m)">外す</button></td>
          </tr>
        </tbody>
      </table>
    </div>
    <h3>グループの権限</h3>
    <p class="muted small">IdP のグループ（ID トークンの groups）に入っている人にまとめて権限を付けます。</p>
    <div v-for="g in s.groups" :key="g.groupId" class="row">
      <b>{{ g.groupName }}</b><span>{{ Roles.label(g.role) }}</span><button v-if="Store.isProjectAdmin" class="ghost small" @click="removeGroup(s, g)">外す</button>
    </div>
    <form v-if="Store.isProjectAdmin" class="row" @submit.prevent="addGroup(s)">
      <input type="text" v-model="s.group" placeholder="グループ名">
      <Combo :bind="Some(s.groupRole)" :options="roleOpts" />
      <button type="submit">追加</button>
    </form>
  </section>
</template>

<style scoped>
.cands { display: flex; flex-wrap: wrap; gap: .4rem; margin: .6rem 0; }
.table-wrap { margin: 1rem 0; }
</style>
