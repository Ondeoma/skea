<script lang="scala">
import io.github.ondeoma.scalactive.components.routes.RouterComponent
import org.scalajs.dom
import skea.shared.*
import skea.web.parts.*
import skea.web.views.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// 画面全体の枠: ヘッダー（プロジェクトのタブ・検索・通知・メニュー）と、ルートごとの画面
case class State(ready: RV[Boolean] = RV(false),
                 menu: RV[Boolean] = RV(false),
                 q: RV[String] = RV(""))

def initState() = State()

val routes: PathPatternRoutes = List(
  ("/", _ => HomeView()),
  ("/login", _ => LoginView()),
  ("/p/:key/*", r => ProjectLayout(key = r.pathname.groups("key"))),
  ("/tasks/:key", r => TaskView(taskKey = r.pathname.groups("key"))),
  ("/search", _ => SearchView()),
  ("/worklogs", _ => WorklogView()),
  ("/notifications", _ => NotificationView()),
  ("/settings", _ => MySettingsView()),
  ("/admin", _ => AdminView()),
)

def mounted(s: State): Unit = {
  Api.clientConfig().foreach { c =>
    Store.config := Some(c)
    // skiwi・skawau と連携していれば、そのリンクに名前を出す
    if (c.linkServices.nonEmpty) LinkTitles.load()
  }
  if (dom.window.location.pathname.startsWith("/login")) s.ready := true
  else Store.report(Api.me()) { me =>
    Store.me := Some(me)
    Store.loadProjects()
    Store.poll()
    Store.connect()
    s.ready := true
  }
}

def search(s: State): Unit = {
  val q = s.q.v.trim
  Router.go("/search?q=" + Api.enc(q))
}

def logout(): Unit = Api.logout().foreach(_ => dom.window.location.href = "/")

/** 表示中のプロジェクトのキー. */
def currentKey(href: String): String = {
  val p = new dom.URL(href).pathname
  if (p.startsWith("/p/")) p.drop(3).takeWhile(_ != '/') else ""
}


def userName = Store.me.mapC(_.map(_.name).getOrElse(""))
def meBrief = Store.me.mapC(_.map(m => UserBrief(m.id, m.name, m.avatarUrl)))
def unreadLabel = Store.unread.mapC(u => if (u.unread > 99) "99+" else u.unread.toString)
def hasUnread = Store.unread.mapC(_.unread > 0)
def loggedIn = Store.me.mapC(_.isDefined)
</script>

<template>
  <header class="top">
    <RouterLink to="/" class="logo"><span class="brand"><img src="/logo.svg" alt="" width="28" height="28">skea</span></RouterLink>
    <div v-if="loggedIn" class="psel">
      <Combo :live="Some(Store.projects.mapC(_.map(p => Opt(p.key, p.key + &quot; &quot; + p.name))))" :shows="Some(Router.href.mapC(currentKey))"
        placeholder="プロジェクト…" :small="true" :onChange='k => Router.go("/p/" + k + "/board")' />
    </div>
    <div class="spacer"></div>
    <form v-if="loggedIn" class="search hide-sp" @submit.prevent="search(s)">
      <input type="search" v-model="s.q" placeholder="タスクを検索（例: ログイン assignee:me）">
    </form>
    <RouterLink v-if="loggedIn" to="/search" class="only-sp"><span class="icon">🔍</span></RouterLink>
    <!-- RouterLink の a には scoped の CSS が届かないので、中の span に当てる -->
    <RouterLink v-if="loggedIn" to="/notifications">
      <span class="icon bell" title="通知">🔔<span v-if="hasUnread" class="dot">{{ unreadLabel }}</span></span>
    </RouterLink>
    <div v-if="loggedIn" class="me">
      <button class="ghost me-btn" @click="s.menu := !s.menu.v"><Avatar :user="meBrief.v" :size="24" /><span class="hide-sp">{{ userName }}</span> ▾</button>
      <div v-if="s.menu" class="menu" @click="s.menu := false">
        <RouterLink to="/worklogs">作業時間の一覧</RouterLink>
        <RouterLink to="/notifications">通知</RouterLink>
        <RouterLink to="/settings">個人設定・APIキー</RouterLink>
        <RouterLink v-if="Store.isAdmin" to="/admin">管理</RouterLink>
        <a href="#" @click.prevent="logout()">ログアウト</a>
      </div>
    </div>
  </header>
  <main>
    <RouterComponent v-if="s.ready" :routes="routes" />
  </main>
  <CreateTaskDialog />
  <DialogHost />
  <div class="toasts">
    <div v-for="t in Store.toasts" :key="t.id" :class='if (t.error) "toast error" else "toast"'>
      <span class="grow" @click="t.link.foreach(l => Router.go(l)); Store.dismiss(t.id)">{{ t.text }}</span>
      <button @click="Store.dismiss(t.id)">×</button>
    </div>
  </div>
</template>

<style scoped>
.top { position: sticky; top: 0; z-index: 100; height: var(--header); display: flex; align-items: center; gap: .6rem; padding: 0 .9rem;
  background: var(--bg); border-bottom: 1px solid var(--line); }
.brand { font-weight: 800; font-size: 1.15rem; color: var(--accent); letter-spacing: .02em; display: inline-flex; align-items: center; gap: .4rem;
  white-space: nowrap; }
.brand img { display: block; border-radius: 7px; }
/* プロジェクトの切り替え（オートコンプリート） */
.psel { width: 16rem; max-width: 45vw; flex: 0 1 auto; }
.spacer { flex: 1; }
.search input { width: 300px; }
.icon { display: inline-block; font-size: 1.1rem; padding: .2rem .4rem; border-radius: 7px; color: var(--fg); position: relative; line-height: 1.2; }
.icon:hover { background: var(--hover); }
.dot { position: absolute; top: -2px; right: -4px; background: var(--danger); color: #fff; border-radius: 999px; font-size: .62rem; padding: 0 .35rem; line-height: 1.4; }
.me { position: relative; }
.menu { position: absolute; right: 0; top: 2.4rem; background: var(--bg); border: 1px solid var(--line); border-radius: 9px; box-shadow: var(--shadow);
  min-width: 200px; padding: .3rem; display: flex; flex-direction: column; }
.menu a { padding: .45rem .7rem; border-radius: 6px; color: var(--fg); }
.menu a:hover { background: var(--hover); text-decoration: none; }
</style>
