<script lang="scala">
import skea.web.*
import io.circe.syntax.*
import org.scalajs.dom
import skea.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// コメント1件（本文・編集・削除・リアクション）
case class Props(c: CommentDto, taskId: UUID, taskKey: String, projectId: UUID, canEdit: Boolean, isAdmin: Boolean,
                 highlight: String, editing: Boolean, onEdit: Option[UUID] => Unit, onChanged: CommentDto => Unit,
                 onRemoved: UUID => Unit)

case class State(editor: RV[Option[EditorHandle]] = RV(None), picker: RV[Boolean] = RV(false))

def initState() = State()

def save(s: State, props: Props): Unit = s.editor.v.foreach { ed =>
  Store.report(Api.updateComment(props.c.id, Editor.fromJs(ed.getJSON()))) { n =>
    props.onEdit(None)
    props.onChanged(n.copy(reactions = props.c.reactions))
  }
}

def remove(props: Props): Unit =
  Dialog.confirm("このコメントを削除しますか？", "削除する") {
    Store.report(Api.delete(s"/api/comments/${props.c.id}")) { _ => props.onRemoved(props.c.id); Store.changed() }
  }

/** コメントの URL（タスクの画面の URL. モーダルで開いていても同じ）. */
def link(props: Props): String = s"/tasks/${props.taskKey}#comment-${props.c.id}"

def copyLink(props: Props): Unit = {
  dom.window.navigator.clipboard.writeText(dom.window.location.origin + link(props))
  // タスクの画面のときだけアドレスバーにも付ける（カンバンなどのモーダルでは URL を変えない）
  if (dom.window.location.pathname == s"/tasks/${props.taskKey}") dom.window.history.replaceState(null, "", s"#comment-${props.c.id}")
  Store.toast("コメントへのリンクをコピーしました")
}

/** リアクションを付ける・外す（自分が付けていれば外す）. */
def react(s: State, props: Props, emoji: String): Unit = {
  s.picker := false
  val on = !props.c.reactions.exists(r => r.emoji == emoji && r.users.exists(u => Store.myId.contains(u.id)))
  val f = if (on) Api.post[List[ReactionDto]](s"/api/comments/${props.c.id}/reactions", ReactionReq(emoji).asJson)
          else Api.send(dom.HttpMethod.DELETE, s"/api/comments/${props.c.id}/reactions?emoji=${Api.enc(emoji)}").flatMap(_ =>
            Api.get[List[CommentDto]](s"/api/tasks/${props.taskId}/comments").map(_.find(_.id == props.c.id).map(_.reactions).getOrElse(Nil)))
  Store.report(f)(rs => props.onChanged(props.c.copy(reactions = rs)))
}

def mine(props: Props): Boolean = props.c.author.exists(a => Store.myId.contains(a.id))
def reacted(r: ReactionDto): Boolean = r.users.exists(u => Store.myId.contains(u.id))
def chipClass(r: ReactionDto): String = if (reacted(r)) "react on" else "react"
def names(r: ReactionDto): String = r.users.map(_.name).mkString("、") + Reactions.customName(r.emoji).fold("")(n => s"（:$n:）")
def cls(props: Props): String = if (props.highlight == s"comment-${props.c.id}") "comment hl" else "comment"
def edited(c: CommentDto): String = if (c.updatedAt.toEpochMilli - c.createdAt.toEpochMilli > 1000) "（編集済み）" else ""
def viaLabel(c: CommentDto): String = c.via match {
  case "automation" => "🤖 自動化"
  case "mcp"        => "🤖 AI（MCP）"
  case "api"        => "API"
  case _            => ""
}
</script>

<template>
  <div :id='"comment-" + props.c.id' :class="cls(props)">
    <Avatar :user="props.c.author" :size="30" />
    <div class="body">
      <div class="head">
        <b>{{ props.c.author.map(_.name).getOrElse("（削除されたユーザー）") }}</b>
        <a :href="link(props)" class="muted small" @click.prevent="copyLink(props)" title="リンクをコピー">{{ Fmt.dateTime(props.c.createdAt) }}</a>
        <span class="muted small">{{ edited(props.c) }} {{ viaLabel(props.c) }}</span>
        <span class="grow"></span>
        <button v-if="mine(props) && props.canEdit" class="ghost small" @click="props.onEdit(Some(props.c.id))">編集</button>
        <button v-if="(mine(props) || props.isAdmin) && props.canEdit" class="ghost small" @click="remove(props)">削除</button>
      </div>
      <div v-if="!props.editing" class="rich" v-html="props.c.html"></div>
      <div v-if="props.editing">
        <RichEditor :content="Some(props.c.body)" :projectId="Some(props.projectId)" :taskId="Some(props.taskId)" :autofocus="true"
          :onReady="h => s.editor := Some(h)" :onSubmit="_ => save(s, props)" />
        <div class="row actions"><button class="primary small" @click="save(s, props)">保存</button><button class="small" @click="props.onEdit(None)">キャンセル</button></div>
      </div>
      <div class="foot">
        <button v-for="r in props.c.reactions" :key="r.emoji" :class="chipClass(r)" :title="names(r)" @click="react(s, props, r.emoji)">
          <img v-if="Emojis.image(r.emoji).isDefined" :src='Emojis.image(r.emoji).getOrElse("")' :alt="r.emoji" class="cemoji">
          <template v-else>{{ r.emoji }}</template> {{ r.users.size }}</button>
        <span class="pick-wrap">
          <button class="ghost small add-react" title="リアクション" @click="s.picker := !s.picker.v">☺＋</button>
          <EmojiPicker v-if="s.picker" :onPick="e => react(s, props, e)" :onClose="_ => s.picker := false" />
        </span>
      </div>
    </div>
  </div>
</template>

<style scoped>
.comment { display: flex; gap: .7rem; padding: .6rem .5rem; border-radius: 8px; }
.comment.hl { background: var(--accent-weak); }
.body { flex: 1; min-width: 0; }
.head { display: flex; gap: .5rem; align-items: center; flex-wrap: wrap; margin-bottom: .2rem; }
.actions { margin-top: .4rem; }
.foot { display: flex; gap: .3rem; align-items: center; flex-wrap: wrap; margin-top: .25rem; }
.react { font-size: .9rem; padding: .12rem .55rem; border-radius: 999px; gap: .3rem; line-height: 1.3; }
.react.on { border-color: var(--accent); background: var(--accent-weak); color: var(--accent); }
.add-react { opacity: .55; font-size: .9rem; }
.comment:hover .add-react { opacity: 1; }
.pick-wrap { position: relative; }
.cemoji { width: 20px; height: 20px; object-fit: contain; vertical-align: -4px; }
</style>
