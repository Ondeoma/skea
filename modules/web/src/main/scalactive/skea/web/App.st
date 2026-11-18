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
  ("/help", _ => HelpHome()),
  ("/help/t/:key", r => HelpTask(taskKey = r.pathname.groups("key"))),
  ("/help/:key", r => HelpPortal(key = r.pathname.groups("key"))),
)

/** 窓口の案内（/help/KEY）はログインしていなくても見られる. */
def isPortal(path: String): Boolean = path.matches("^/help/[^/]+/?$") && !path.startsWith("/help/t/")

/** ログインしている人の情報を読み、プロジェクトと窓口を読む. 問い合わせする"ユーザ"だけの人は窓口の画面にする. */
def start(s: State, me: MeDto): Unit = {
  Store.me := Some(me)
  Observe.setUser(me.id.toString)
  Store.loadAll().onComplete { r =>
    val path = dom.window.location.pathname
    if (r.toOption.contains(true)) {
      if (path == "/") Router.go("/help")
      else if (path.startsWith("/tasks/")) Router.go("/help/t/" + path.stripPrefix("/tasks/") + dom.window.location.hash)
    }
    s.ready := true
  }
  Store.poll()
  Store.connect()
}

def mounted(s: State): Unit = {
  Fallback.install(routes)
  Api.clientConfig().foreach { c =>
    Store.config := Some(c)
    // skiwi・skawau と連携していれば、そのリンクに名前を出す（ログインせずに見られる窓口の案内では読まない）
    if (c.linkServices.nonEmpty && !isPortal(dom.window.location.pathname)) LinkTitles.load()
  }
  val path = dom.window.location.pathname
  if (path.startsWith("/login")) s.ready := true
  else if (isPortal(path)) Api.meOpt().foreach {
    case Some(me) => start(s, me)
    case None     => s.ready := true
  }
  else Store.report(Api.me())(me => start(s, me))
}

def search(s: State): Unit = {
  val q = s.q.v.trim
  Router.go("/search?q=" + Api.enc(q))
}

def logout(): Unit = Api.logout().onComplete(r => dom.window.location.href = r.getOrElse("/"))

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
/** エージェントとしての画面（プロジェクトの切り替え・検索）を出すか. */
def agentUi = computed { Store.me.v.isDefined && !Store.helpOnly.v }
def hasPortals = Store.helpProjects.mapC(_.nonEmpty)
</script>

<template>
  <header class="top">
    <RouterLink v-if="Store.helpOnly.mapC(!_)" to="/" class="logo"><span class="brand"><img src="/logo.svg" alt="" width="28" height="28">skea</span></RouterLink>
    <RouterLink v-if="Store.helpOnly" to="/help" class="logo"><span class="brand"><img src="/logo.svg" alt="" width="28" height="28">skea</span></RouterLink>
    <div v-if="agentUi" class="psel">
      <Combo :live="Some(Store.projects.mapC(_.map(p => Opt(p.key, p.key + &quot; &quot; + p.name))))" :shows="Some(Router.href.mapC(currentKey))"
        placeholder="プロジェクト…" :small="true" :onChange='k => Router.go("/p/" + k + "/board")' />
    </div>
    <div class="spacer"></div>
    <form v-if="agentUi" class="search hide-sp" @submit.prevent="search(s)">
      <input type="search" v-model="s.q" placeholder="タスクを検索（例: ログイン assignee:me）">
    </form>
    <RouterLink v-if="agentUi" to="/search" class="only-sp"><span class="icon">🔍</span></RouterLink>
    <!-- RouterLink の a には scoped の CSS が届かないので、中の span に当てる -->
    <RouterLink v-if="loggedIn" to="/notifications">
      <span class="icon bell" title="通知">🔔<span v-if="hasUnread" class="dot">{{ unreadLabel }}</span></span>
    </RouterLink>
    <div v-if="loggedIn" class="me">
      <button class="ghost me-btn" @click="s.menu := !s.menu.v"><Avatar :user="meBrief.v" :size="24" /><span class="hide-sp">{{ userName }}</span> ▾</button>
      <div v-if="s.menu" class="menu-bg" @click="s.menu := false"></div>
      <div v-if="s.menu" class="menu" @click="s.menu := false">
        <RouterLink v-if="hasPortals" to="/help">問い合わせ窓口</RouterLink>
        <RouterLink v-if="agentUi" to="/worklogs">作業時間の一覧</RouterLink>
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
  min-width: 200px; padding: .3rem; display: flex; flex-direction: column; z-index: 200; }
.menu-bg { position: fixed; inset: 0; z-index: 199; }
</style>
