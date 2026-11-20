<script lang="scala">
import skea.web.*
import skea.web.parts.*
import io.circe.syntax.*
import org.scalajs.dom
import skea.shared.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

// ヘルプデスクの問い合わせ（/help/t/KEY）: 内容・ステータス・やりとり（公開のコメントだけ）・返信・添付.
// エージェントの内部メモ・作業時間・ラベルなどは出さない（API も返さない）.
case class Props(taskKey: String)

case class State(detail: RV[Option[HelpTaskDetailDto]] = RV(None),
                 error: RV[String] = RV(""),
                 editor: RV[Option[EditorHandle]] = RV(None),
                 act: ActionState,
                 upload: ActionState,
                 shares: RV[Option[HelpSharesDto]] = RV(None))

def initState() = State(act = useAction(), upload = useAction())

def load(s: State, props: Props): Unit =
  Api.helpTask(props.taskKey).onComplete {
    case Success(d) =>
      s.detail := Some(d)
      pageTitle(s"${d.task.key} ${d.task.title}")
      val h = dom.window.location.hash
      if (h.startsWith("#comment-")) dom.window.setTimeout(() => Option(dom.document.getElementById(h.drop(1))).foreach(_.scrollIntoView(true)), 150)
    case Failure(e) => s.error := e.getMessage
  }

def loadShares(s: State, props: Props): Unit =
  Api.get[HelpSharesDto](s"/api/help/tasks/${Api.enc(props.taskKey)}/shares").foreach(x => s.shares := Some(x))

/** 自分の共有をこの一覧にする（エージェントが共有したものは残る）. */
def saveShares(s: State, props: Props, ids: List[java.util.UUID], msg: String): Unit =
  Toasts.report(Api.put[HelpSharesDto](s"/api/help/tasks/${Api.enc(props.taskKey)}/shares", HelpSharesReq(ids).asJson)) { x =>
    s.shares := Some(x)
    Toasts.show(msg)
    load(s, props)
  }

def mineIds(s: State): List[java.util.UUID] = s.shares.v.toList.flatMap(_.current.filter(_.removable).map(_.groupId))

def addShare(s: State, props: Props, v: String): Unit = if (v.nonEmpty)
  saveShares(s, props, (mineIds(s) :+ java.util.UUID.fromString(v)).distinct, "共有しました（グループの人に通知し、ウォッチャーにも登録しました）")

def removeShare(s: State, props: Props, id: java.util.UUID): Unit =
  saveShares(s, props, mineIds(s).filterNot(_ == id), "共有をやめました")

def shareOpts(s: State) = s.shares.mapC(_.toList.flatMap(_.options).map(o => Opt(o.groupId.toString, o.name, icon = "👥")))
def canShare(s: State) = s.shares.mapC(_.exists(_.canShare))
def showShares(s: State) = s.shares.mapC(_.exists(x => x.canShare || x.current.nonEmpty))

def mounted(s: State, props: Props): Unit = {
  load(s, props)
  loadShares(s, props)
  watch(Store.helpTick, false)((_, _) => load(s, props))
}

def reply(s: State, props: Props): Unit = s.editor.v.foreach { ed =>
  if (!ed.isEmpty() && !s.act.busy.v)
    Toasts.report(s.act.run(Api.helpReply(props.taskKey, Editor.fromJs(ed.getJSON())))) { c =>
      ed.clear()
      s.detail := s.detail.v.map(d => d.copy(comments = d.comments :+ c))
    }
}

def upload(s: State, props: Props, e: dom.Event): Unit = s.detail.v.foreach { d =>
  s.upload.run(scala.concurrent.Future.sequence(Dom.takeFiles(e).map(Api.helpUpload(d.task.id, _)))).onComplete { r =>
    r.failed.foreach(Toasts.error)
    load(s, props)
  }
}

def watchToggle(s: State, props: Props): Unit = s.detail.v.foreach { d =>
  val url = s"/api/help/tasks/${d.task.key}/watch"
  Toasts.report(if (d.watching) Api.delete(url) else Api.postUnit(url))(_ => load(s, props))
}

def mine(c: CommentDto): Boolean = c.author.exists(a => Store.myId.contains(a.id))
def bubble(c: CommentDto): String = if (mine(c)) "msg mine" else "msg"
</script>

<template>
  <div class="page narrow help">
    <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
    <template v-for="d in s.detail.mapC(_.toList)">
      <p class="small"><RouterLink :to='"/help/" + d.projectKey'>← {{ d.projectName }}</RouterLink></p>
      <div class="row top">
        <span class="badge" :style="HelpGroups.statusStyle(d.task)">{{ d.task.statusName }}</span>
        <h1 class="grow">{{ d.task.title }}</h1>
      </div>
      <p class="muted small">
        {{ d.task.key }} ・ {{ d.task.reporter.map(_.name).getOrElse("") }} さんの問い合わせ ・ {{ Fmt.dateTime(d.task.createdAt) }}
        <template v-if="d.task.assignee.isDefined"> ・ 担当 {{ d.task.assignee.map(_.name).getOrElse("") }}</template>
      </p>
      <p v-if="d.agent" class="muted small">エージェントとして表示しています。内部メモはここには出ません。<RouterLink :to='"/tasks/" + d.task.key'>タスクの画面へ →</RouterLink></p>

      <dl v-if="d.fields.nonEmpty" class="card fields">
        <template v-for="f in d.fields"><dt class="muted small">{{ f.name }}</dt><dd>{{ HelpFields.show(f.value) }}</dd></template>
      </dl>
      <div v-if="d.descriptionHtml.nonEmpty" class="card rich desc" v-html="d.descriptionHtml" v-rich="d.descriptionHtml"></div>

      <section v-if="d.attachments.nonEmpty" class="atts">
        <a v-for="a in d.attachments" :key="a.id" :href="a.url" target="_blank" rel="noopener" class="att">
          <img v-if='a.contentType.startsWith("image/")' :src="a.url" alt="" loading="lazy">
          <span v-else>📄</span> <span class="small">{{ a.fileName }}</span>
        </a>
      </section>

      <section v-if="showShares(s)" class="card shares">
        <div class="row"><b>共有</b><span class="muted small grow">共有したグループの人もこの問い合わせを見られ、更新の通知が届きます</span></div>
        <div class="chips">
          <span v-for="c in s.shares.mapC(_.toList.flatMap(_.current))" :key="c.groupId" class="chip">👥 {{ c.name }}
            <button v-if="c.removable" class="ghost small x" @click="removeShare(s, props, c.groupId)" title="共有をやめる">×</button></span>
          <span v-if="s.shares.mapC(_.exists(_.current.isEmpty))" class="muted small">まだ共有していません</span>
        </div>
        <Combo v-if="canShare(s)" :live="Some(shareOpts(s))" :small="true" :keep="false" placeholder="＋ 同じドメインのグループに共有"
          :onChange="v => addShare(s, props, v)" />
      </section>

      <h2>やりとり</h2>
      <div class="thread">
        <div v-for="c in d.comments" :key="c.id" :id='"comment-" + c.id' :class="bubble(c)">
          <div class="who"><Avatar :user="c.author.map(person)" :size="22" /> <b>{{ c.author.map(_.name).getOrElse("") }}</b>
            <span class="muted small">{{ Fmt.dateTime(c.createdAt) }}</span></div>
          <div class="rich" v-html="c.html" v-rich="c.html"></div>
        </div>
        <p v-if="d.comments.isEmpty" class="muted">まだ返信はありません。担当者から返信があると通知でお知らせします。</p>
      </div>

      <div class="reply">
        <RichEditor placeholder="返信を書く（画像は貼り付けで添付、Ctrl+Enter で送信）" :taskId="Some(d.task.id)" :uploadUrl='Some("/api/help/tasks/" + d.task.id + "/attachments")'
          :onReady="h => s.editor := Some(h)" :onSubmit="_ => reply(s, props)" />
        <div class="row actions">
          <label class="btn small">📎 ファイルを添付<input type="file" multiple class="hidden" @change="upload(s, props, event)"></label>
          <span v-if="s.upload.busy" class="muted small">アップロード中…</span>
          <span class="grow"></span>
          <button class="small" @click="watchToggle(s, props)">{{ if (d.watching) "🔔 通知を受け取っています" else "🔕 通知を受け取る" }}</button>
          <button class="primary" :disabled="s.act.busy" @click="reply(s, props)">返信する</button>
        </div>
      </div>
    </template>
  </div>
</template>

<style scoped>
.top { align-items: center; gap: .7rem; }
.top h1 { margin: .2rem 0; }
.badge { flex: none; color: #fff; font-size: .8rem; padding: .2rem .7rem; border-radius: 999px; white-space: nowrap; }
.desc { padding: 1rem; margin: 1rem 0; }
.atts { display: flex; flex-wrap: wrap; gap: .6rem; margin: .6rem 0 1rem; }
.att { display: inline-flex; align-items: center; gap: .3rem; border: 1px solid var(--line); border-radius: 8px; padding: .3rem .5rem; color: var(--fg); }
.att img { width: 56px; height: 56px; object-fit: cover; border-radius: 4px; }
.thread { display: flex; flex-direction: column; gap: .7rem; margin-bottom: 1rem; }
.msg { border: 1px solid var(--line); border-radius: 12px; padding: .6rem .9rem; background: var(--card); max-width: 92%; }
.msg.mine { align-self: flex-end; background: var(--accent-weak); border-color: transparent; }
.who { display: flex; align-items: center; gap: .4rem; margin-bottom: .3rem; }
.reply { margin-top: 1rem; }
.fields { display: grid; grid-template-columns: max-content 1fr; gap: .3rem 1rem; padding: .8rem 1rem; margin: 1rem 0 0; }
.fields dt { margin: 0; } .fields dd { margin: 0; white-space: pre-wrap; }
.shares { padding: .7rem 1rem; margin: .5rem 0 1rem; display: flex; flex-direction: column; gap: .4rem; }
.chips { display: flex; flex-wrap: wrap; gap: .35rem; align-items: center; }
.x { padding: 0 .2rem; margin-left: .1rem; }
.actions { margin-top: .5rem; gap: .5rem; align-items: center; flex-wrap: wrap; }
</style>
