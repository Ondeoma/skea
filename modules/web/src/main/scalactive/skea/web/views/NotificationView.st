<script lang="scala">
import skea.web.*
import io.circe.syntax.*
import skea.shared.*
import skea.web.parts.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// 画面内の通知の一覧（種類・未読で絞り込む）
case class State(kind: RV[String] = RV(""), unread: RV[Boolean] = RV(false), list: RV[List[NotificationDto]] = RV(Nil))

def initState() = State()

def load(s: State): Unit =
  Api.notifications(Option(s.kind.v).filter(_.nonEmpty).toList, s.unread.v).foreach(r => s.list := r.items)

def mounted(s: State): Unit = {
  pageTitle("通知")
  load(s)
  watch(Store.unread, false)((_, _) => load(s))
}

def pick(s: State, k: String): Unit = { s.kind := k; load(s) }

def open(n: NotificationDto): Unit = {
  if (!n.read) Api.postUnit("/api/notifications/read", MarkReadReq(List(n.id)).asJson).foreach(_ => Store.poll())
  n.taskKey.foreach(k => Router.go(s"/tasks/$k" + n.commentId.fold("")(c => s"#comment-$c")))
}

def readAll(s: State): Unit =
  Store.report(Api.postUnit("/api/notifications/read-all" + Option(s.kind.v).filter(_.nonEmpty).fold("")(k => s"?kinds=$k"))) { _ =>
    load(s); Store.poll()
  }

def chip(s: State, k: String) = s.kind.mapC(x => if (x == k) "chip active" else "chip")
def icon(k: String): String = k match {
  case NotificationKind.Mention    => "＠"
  case NotificationKind.Assigned   => "👤"
  case NotificationKind.Comment    => "💬"
  case NotificationKind.Status     => "🔄"
  case NotificationKind.Due        => "⏰"
  case NotificationKind.Automation => "⚡"
  case _                           => "✏️"
}
def mentions = Store.unread.mapC(u => if (u.mentions > 0) s"メンション（${u.mentions}）" else "メンション")
</script>

<template>
  <div class="page narrow">
    <div class="row"><h1>通知</h1><span class="grow"></span><button class="small" @click="readAll(s)">すべて既読にする</button></div>
    <div class="row filters">
      <div class="chips">
        <button :class='chip(s, "")' @click='pick(s, "")'>すべて</button>
        <button :class="chip(s, NotificationKind.Mention)" @click="pick(s, NotificationKind.Mention)">{{ mentions }}</button>
        <button v-for="k in NotificationKind.all.filterNot(_ == NotificationKind.Mention)" :class="chip(s, k)" @click="pick(s, k)">{{ NotificationKind.label(k) }}</button>
      </div>
      <label class="small"><input type="checkbox" v-model="s.unread" @change="load(s)"> 未読のみ</label>
    </div>
    <div class="list">
      <div v-for="n in s.list" :key="n.id" :class='if (n.read) "item" else "item unread"' @click="open(n)">
        <span class="icon">{{ icon(n.kind) }}</span>
        <div class="grow">
          <div>{{ n.title }}</div>
          <div class="muted small body">{{ n.body }}</div>
          <div class="muted small"><Avatar :user="n.actor" :size="16" /> {{ n.actor.map(_.name).getOrElse("skea") }} · {{ Fmt.ago(n.at) }}</div>
        </div>
      </div>
      <p v-if="s.list.mapC(_.isEmpty)" class="empty">通知はありません</p>
    </div>
  </div>
</template>

<style scoped>
.filters { margin-bottom: 1rem; justify-content: space-between; }
.item { display: flex; gap: .7rem; padding: .7rem .6rem; border-bottom: 1px solid var(--line); cursor: pointer; border-radius: 6px; }
.item:hover { background: var(--hover); }
.item.unread { background: var(--accent-weak); }
.icon { width: 1.6rem; text-align: center; font-size: 1.1rem; }
.body { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 60ch; }
</style>
