<script lang="scala">
import skea.web.*
import org.scalajs.dom
import skea.shared.*

import io.circe.Json

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

// タスクのコメント（アンカーリンク・編集・削除）と、新しいコメントの入力.
// 入力中のコメントは、少し間を置いてサーバーに下書きとして保存する（ユーザー＋タスクごと. 投稿すると消える）.
// ヘルプデスクのプロジェクトでは、エージェントだけに見える内部メモとしても投稿できる（"ユーザ"には見えず、通知もしない）.
case class Props(taskId: UUID, taskKey: String, projectId: UUID, canEdit: Boolean, isAdmin: Boolean)

case class State(list: RV[List[CommentDto]] = RV(Nil),
                 editor: RV[Option[EditorHandle]] = RV(None),
                 editing: RV[Option[UUID]] = RV(None),
                 busy: RV[Boolean] = RV(false),
                 highlight: RV[String] = RV(""),
                 draft: RV[Option[Option[Json]]] = RV(None),
                 draftNote: RV[String] = RV(""),
                 pending: RV[Option[(Int, Json)]] = RV(None),
                 internal: RV[Boolean] = RV(false))

def initState() = State()

def load(s: State, props: Props, scroll: Boolean = false): Unit =
  Api.comments(props.taskId).foreach { cs =>
    s.list := cs
    if (scroll) dom.window.setTimeout(() => scrollToHash(s), 150)
  }

def scrollToHash(s: State): Unit = {
  val h = dom.window.location.hash
  if (h.startsWith("#comment-")) {
    s.highlight := h.drop(1)
    Option(dom.document.getElementById(h.drop(1))).foreach(_.scrollIntoView(true))
    dom.window.scrollBy(0, -120)
  }
}

def mounted(s: State, props: Props): Unit = {
  load(s, props, scroll = true)
  // 下書きを読んでから入力欄を出す（読めなくても空の入力欄は出す）
  if (props.canEdit) Api.draft(props.taskId).onComplete { r =>
    val d = r.toOption.flatten
    s.draft := Some(d.map(_.body))
    d.foreach(x => s.draftNote := s"下書きを復元しました（${Fmt.ago(x.updatedAt)}に保存）")
  }
  startWatch(s, props)
}

def unmounted(s: State, props: Props): Unit = flush(s, props)

/** 入力が止まって 1.5 秒後に下書きを保存する. */
def changed(s: State, props: Props, body: Json): Unit = {
  s.pending.v.foreach(p => dom.window.clearTimeout(p._1))
  val t = dom.window.setTimeout(() => flush(s, props), 1500)
  s.pending := Some((t, body))
  s.draftNote := "入力中…"
}

def flush(s: State, props: Props): Unit = s.pending.v.foreach { p =>
  dom.window.clearTimeout(p._1)
  s.pending := None
  Api.saveDraft(props.taskId, p._2).onComplete {
    case Success(_) => s.draftNote := s"下書きを保存しました（${Fmt.clock()}）"
    case Failure(_) => s.draftNote := "下書きを保存できませんでした"
  }
}

def discard(s: State, props: Props): Unit = Dialog.confirm("入力中のコメント（下書き）を破棄しますか？", "破棄する") {
  s.pending.v.foreach(p => dom.window.clearTimeout(p._1))
  s.pending := None
  s.editor.v.foreach(_.clear())
  Api.delete(s"/api/tasks/${props.taskId}/draft").foreach(_ => s.draftNote := "")
}

def startWatch(s: State, props: Props): Unit = {
  watch(Store.tick, false)((_, _) => if (s.editing.v.isEmpty) load(s, props))
}

def post(s: State, props: Props): Unit = s.editor.v.foreach { ed =>
  if (!ed.isEmpty() && !s.busy.v) {
    s.busy := true
    s.pending.v.foreach(p => dom.window.clearTimeout(p._1))
    s.pending := None
    Api.addComment(props.taskId, Editor.fromJs(ed.getJSON()), Store.helpdesk.v && s.internal.v).onComplete {
      case Success(c) =>
        s.busy := false
        s.draftNote := ""
        ed.clear()
        s.list := s.list.v :+ c
        Store.changed()
      case Failure(e) =>
        s.busy := false
        Store.error(e)
    }
  }
}

def replace(s: State, c: CommentDto): Unit = s.list := s.list.v.map(x => if (x.id == c.id) c else x)
def drop(s: State, id: UUID): Unit = s.list := s.list.v.filterNot(_.id == id)
def isEditing(s: State, c: CommentDto): Boolean = s.editing.v.contains(c.id)
def postLabel(s: State) = s.internal.mapC(i => if (i && Store.helpdesk.v) "内部メモを投稿" else "コメントする")
def newClass(s: State) = s.internal.mapC(i => if (i && Store.helpdesk.v) "new internal" else "new")
</script>

<template>
  <div class="comments">
    <CommentItem v-for="c in s.list" :key="c.id" :c="c" :taskId="props.taskId" :taskKey="props.taskKey" :projectId="props.projectId"
      :canEdit="props.canEdit" :isAdmin="props.isAdmin" :highlight="s.highlight.v" :editing="isEditing(s, c)" :onEdit="e => s.editing := e"
      :onChanged="n => replace(s, n)" :onRemoved="id => drop(s, id)" />
    <p v-if="s.list.mapC(_.isEmpty)" class="muted small">コメントはまだありません</p>
    <div v-for="d in s.draft.mapC(_.toList)" :class="newClass(s)">
      <RichEditor placeholder="コメントを書く（@でメンション、/でメニュー、画像は貼り付けで添付、Ctrl+Enter で送信）" :content="d"
        :projectId="Some(props.projectId)" :taskId="Some(props.taskId)"
        :onReady="h => s.editor := Some(h)" :onSubmit="_ => post(s, props)" :onChange="j => changed(s, props, j)" />
      <div class="row actions">
        <label v-if="Store.helpdesk" class="small internal-toggle" title="問い合わせした&quot;ユーザ&quot;には見えず、通知もしません">
          <input type="checkbox" :checked="s.internal.v" @change="s.internal := Dom.checked(event)"> 🔒 内部メモ（エージェントのみ）</label>
        <span class="muted small grow">{{ s.draftNote }}</span>
        <button v-if="s.draftNote.mapC(_.nonEmpty)" class="ghost small" @click="discard(s, props)">下書きを破棄</button>
        <button class="primary" :disabled="s.busy" @click="post(s, props)">{{ postLabel(s) }}</button>
      </div>
    </div>
  </div>
</template>

<style scoped>
.new { margin-top: 1rem; }
.new.internal { background: var(--internal-bg); border-radius: 8px; padding: .4rem; }
.internal-toggle { display: inline-flex; align-items: center; gap: .3rem; white-space: nowrap; }
.actions { margin-top: .4rem; }
</style>
