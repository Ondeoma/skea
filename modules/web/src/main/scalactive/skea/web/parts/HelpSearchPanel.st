<script lang="scala">
import skea.web.*
import org.scalajs.dom
import skea.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

// ヘルプデスクの窓口の検索: 検索欄（キーワードと絞り込みの構文）、よく使う絞り込み、詳細条件（入力欄から構文を組み立てる）、結果.
// 条件は URL（?q=）に入れるので、再読み込み・戻る・URL の共有で同じ結果になる. 探せるのは自分に見える問い合わせだけ.
case class Props(projectId: UUID, projectKey: String)

case class State(q: RV[String] = RV("is:open"),
                 hits: RV[List[HelpHitDto]] = RV(Nil),
                 total: RV[Int] = RV(0),
                 error: RV[String] = RV(""),
                 loading: RV[Boolean] = RV(false),
                 advanced: RV[Boolean] = RV(false),
                 syntax: RV[Boolean] = RV(false),
                 statuses: RV[List[StatusDto]] = RV(Nil),
                 // 詳細条件
                 words: RV[String] = RV(""),
                 scope: RV[String] = RV(""),
                 cats: RV[List[String]] = RV(Nil),
                 sts: RV[List[String]] = RV(Nil),
                 assignee: RV[String] = RV(""),
                 reporter: RV[String] = RV(""),
                 createdFrom: RV[String] = RV(""), createdTo: RV[String] = RV(""),
                 updatedFrom: RV[String] = RV(""), updatedTo: RV[String] = RV(""),
                 attachment: RV[Boolean] = RV(false),
                 replies: RV[Boolean] = RV(false),
                 watching: RV[Boolean] = RV(false),
                 order: RV[String] = RV("-updated"))

val PageSize = 50

def initState() = State(q = RV(Router.queryParam("q").v.getOrElse("is:open")))

def mounted(s: State, props: Props): Unit = {
  run(s, props)
  Api.get[List[StatusDto]](s"/api/help/projects/${props.projectId}/statuses").foreach(s.statuses := _)
  // 自分への通知が届いたら（返信・ステータスの変更など）結果を取り直す
  watch(Store.helpTick, false)((_, _) => run(s, props))
}

/** 検索する（more なら続きを足す）. 条件は URL にも入れる. */
def run(s: State, props: Props, more: Boolean = false): Unit = {
  val q = s.q.v.trim
  val offset = if (more) s.hits.v.size else 0
  s.loading := true
  dom.window.history.replaceState(null, "", s"/help/${props.projectKey}" + (if (q == "is:open") "" else "?q=" + js.URIUtils.encodeURIComponent(q)))
  Api.get[HelpSearchResultDto](s"/api/help/projects/${props.projectId}/search" + Api.query("q" -> Some(q), "limit" -> Some(PageSize.toString),
    "offset" -> Some(offset.toString))).onComplete { r =>
    s.loading := false
    r.fold(Toasts.error, res => {
      s.error := res.error.getOrElse("")
      s.hits := (if (more) s.hits.v ++ res.hits else res.hits)
      s.total := res.total
    })
  }
}

def preset(s: State, props: Props, q: String): Unit = { s.q := q; run(s, props) }

def cond(s: State): HelpCond = HelpCond(s.words.v, s.scope.v, s.cats.v, s.sts.v, s.assignee.v, s.reporter.v, s.createdFrom.v, s.createdTo.v,
  s.updatedFrom.v, s.updatedTo.v, s.attachment.v, s.replies.v, s.watching.v, s.order.v)

/** 詳細条件を検索欄の構文にして検索する. */
def apply(s: State, props: Props): Unit = { s.q := HelpQuery.build(cond(s)); run(s, props) }

def clear(s: State, props: Props): Unit = {
  List(s.words, s.scope, s.assignee, s.reporter, s.createdFrom, s.createdTo, s.updatedFrom, s.updatedTo).foreach(_ := "")
  s.cats := Nil; s.sts := Nil; s.attachment := false; s.replies := false; s.watching := false; s.order := "-updated"
  preset(s, props, "is:open")
}

def toggle(xs: RV[List[String]], v: String): Unit = xs := (if (xs.v.contains(v)) xs.v.filterNot(_ == v) else xs.v :+ v)
def chipClass(xs: RV[List[String]], v: String) = xs.mapC(l => if (l.contains(v)) "chip active" else "chip")
def presetClass(s: State, q: String) = s.q.mapC(x => if (x.trim == q) "chip active" else "chip")
def scopeClass(s: State, v: String) = s.scope.mapC(x => if (x == v) "chip active" else "chip")
def preview(s: State) = computed { HelpQuery.build(cond(s)) }
def summary(s: State) = computed {
  if (s.loading.v) "検索中…" else if (s.error.v.nonEmpty) "" else s"${s.total.v} 件" + (if (s.hits.v.exists(_.fuzzy)) "（似た語の一致を含む）" else "")
}
def hasMore(s: State) = computed { !s.loading.v && s.hits.v.count(!_.fuzzy) < s.total.v - s.hits.v.count(_.fuzzy) }
</script>

<template>
  <div class="hsearch">
    <form class="row bar" @submit.prevent="run(s, props)">
      <input type="search" v-model="s.q" class="grow q" placeholder="キーワードと条件（例: VPN status:進行中 created>-30d）" aria-label="問い合わせを検索">
      <button type="submit" class="primary">検索</button>
      <button type="button" :class='s.advanced.mapC(a => if (a) "small active" else "small")' @click="s.advanced := !s.advanced.v">詳細条件 {{ s.advanced.mapC(a => if (a) "▴" else "▾") }}</button>
    </form>
    <div class="chips presets">
      <button v-for="p in HelpQuery.presets" :key="p._1" :class="presetClass(s, p._1)" @click="preset(s, props, p._1)">{{ p._2 }}</button>
    </div>

    <form v-if="s.advanced" class="card adv" @submit.prevent="apply(s, props)">
      <div class="grid">
        <label class="field wide"><span>キーワード（件名・内容・返信。"..." で語句、-語 で除外）</span>
          <input type="text" v-model="s.words" placeholder="例: VPN &quot;認証に失敗&quot; -解決済み"></label>
        <div class="field wide"><span>範囲</span>
          <div class="chips">
            <button type="button" :class='scopeClass(s, "")' @click='s.scope := ""'>すべて</button>
            <button type="button" :class='scopeClass(s, "mine")' @click='s.scope := "mine"'>自分の問い合わせ</button>
            <button type="button" :class='scopeClass(s, "shared")' @click='s.scope := "shared"'>共有されたもの</button>
          </div></div>
        <div class="field wide"><span>状況</span>
          <div class="chips">
            <button v-for="c in HelpQuery.categories" :key="c._1" type="button" :class="chipClass(s.cats, c._1)" @click="toggle(s.cats, c._1)">{{ c._2 }}</button>
          </div></div>
        <div v-if="s.statuses.mapC(_.nonEmpty)" class="field wide"><span>ステータス</span>
          <div class="chips">
            <button v-for="st in s.statuses" :key="st.id" type="button" :class="chipClass(s.sts, st.name)" @click="toggle(s.sts, st.name)">
              <span class="dot" :style='"background:" + st.color'></span>{{ st.name }}</button>
          </div></div>
        <label class="field"><span>担当者（名前。未割り当ては none）</span><input type="text" v-model="s.assignee"></label>
        <label class="field"><span>問い合わせた人（名前。自分は me）</span><input type="text" v-model="s.reporter"></label>
        <div class="field"><span>作成日</span>
          <div class="row range"><input type="date" v-model="s.createdFrom"><span>〜</span><input type="date" v-model="s.createdTo"></div></div>
        <div class="field"><span>更新日</span>
          <div class="row range"><input type="date" v-model="s.updatedFrom"><span>〜</span><input type="date" v-model="s.updatedTo"></div></div>
        <div class="field wide"><span>そのほか</span>
          <div class="row checks">
            <label><input type="checkbox" :checked="s.attachment.v" @change="s.attachment := Dom.checked(event)"> 添付がある</label>
            <label><input type="checkbox" :checked="s.replies.v" @change="s.replies := Dom.checked(event)"> 返信がある</label>
            <label><input type="checkbox" :checked="s.watching.v" @change="s.watching := Dom.checked(event)"> 通知を受け取っている</label>
          </div></div>
        <label class="field"><span>並び順</span>
          <select v-model="s.order">
            <option v-for="o in HelpQuery.orders" :key="o._1" :value="o._1">{{ o._2 }}</option>
          </select></label>
      </div>
      <p class="muted small">検索欄の条件: <code>{{ preview(s) }}</code></p>
      <div class="row actions">
        <button type="button" class="ghost small" @click="s.syntax := !s.syntax.v">書き方の一覧</button>
        <span class="grow"></span>
        <button type="button" @click="clear(s, props)">クリア</button>
        <button type="submit" class="primary">この条件で検索</button>
      </div>
      <table v-if="s.syntax" class="syntax small">
        <tbody>
          <tr v-for="k in HelpSearch.keys" :key="k._1"><td><code>{{ k._1 }}</code></td><td class="muted">{{ k._2 }}</td></tr>
        </tbody>
      </table>
    </form>

    <div class="row head">
      <h2 class="grow">問い合わせ</h2>
      <span class="muted small">{{ summary(s) }}</span>
    </div>
    <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
    <div class="tickets">
      <div v-for="h in s.hits" :key="h.task.id" class="ticket" @click='Router.go("/help/t/" + h.task.key)'>
        <span class="badge" :style="HelpGroups.statusStyle(h.task)">{{ h.task.statusName }}</span>
        <span class="grow">
          <span class="ttl">{{ h.task.title }}</span>
          <span v-if="h.snippet.isDefined" class="snippet small" v-html='h.snippet.getOrElse("")'></span>
          <span class="muted small">{{ h.task.key }} ・ {{ if (h.task.mine) "自分の問い合わせ" else "共有されたもの" }}
            <template v-if="h.task.assignee.isDefined"> ・ 担当 {{ h.task.assignee.map(_.name).getOrElse("") }}</template>
            ・ 更新 {{ Fmt.ago(h.task.updatedAt) }}<template v-if="h.fuzzy"> ・ 似た語の一致</template></span>
        </span>
      </div>
      <p v-if="computed { s.hits.v.isEmpty && !s.loading.v && s.error.v.isEmpty }" class="muted">条件に合う問い合わせはありません</p>
      <button v-if="hasMore(s)" class="more" @click="run(s, props, more = true)">もっと見る</button>
    </div>
  </div>
</template>

<style scoped>
.bar { gap: .5rem; margin-top: 1.2rem; }
.q { min-width: 0; }
.presets { margin: .6rem 0; }
.chips { display: flex; flex-wrap: wrap; gap: .35rem; }
.adv { padding: 1rem; margin: .4rem 0 1rem; }
.grid { display: grid; grid-template-columns: 1fr 1fr; gap: .2rem 1rem; }
.wide { grid-column: 1 / -1; }
@media (max-width: 640px) { .grid { grid-template-columns: 1fr; } }
.range { gap: .4rem; align-items: center; }
.range input { min-width: 0; flex: 1; }
.checks { gap: 1rem; flex-wrap: wrap; }
.checks label { display: inline-flex; align-items: center; gap: .3rem; }
.dot { display: inline-block; width: .55rem; height: .55rem; border-radius: 50%; margin-right: .3rem; }
.actions { gap: .5rem; margin-top: .4rem; }
.syntax { margin-top: .6rem; border-collapse: collapse; }
.syntax td { padding: .15rem .6rem .15rem 0; vertical-align: top; }
code { overflow-wrap: anywhere; }
.head { margin-top: 1rem; align-items: center; }
.tickets { display: flex; flex-direction: column; gap: .5rem; }
.ticket { display: flex; gap: .8rem; align-items: center; padding: .8rem 1rem; border: 1px solid var(--line); border-radius: 10px; color: var(--fg);
  background: var(--card); cursor: pointer; }
.ticket:hover { border-color: var(--accent); }
.ticket .grow { display: flex; flex-direction: column; min-width: 0; gap: .1rem; }
.ttl { font-weight: 600; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.snippet { color: var(--fg); opacity: .8; }
.badge { flex: none; color: #fff; font-size: .75rem; padding: .15rem .6rem; border-radius: 999px; white-space: nowrap; }
.more { align-self: center; margin-top: .4rem; }
</style>
