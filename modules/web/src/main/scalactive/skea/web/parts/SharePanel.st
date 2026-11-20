<script lang="scala">
import skea.web.*
import io.circe.syntax.*
import skea.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// ヘルプデスク: タスクを問い合わせする"ユーザ"に共有する（グループ・個人）. 親のグループに共有すると子のグループの人にも見える.
// 「共有先に通知する」なら、新しく見えるようになった人に画面内・メール・プッシュで知らせる.
case class Props(taskId: UUID, projectId: UUID, canEdit: Boolean, onChanged: Unit => Unit = _ => ())

case class State(shares: RV[TaskSharesDto] = RV(TaskSharesDto(Nil, Nil)),
                 settings: RV[Option[HelpdeskSettingsDto]] = RV(None),
                 notifyNew: RV[Boolean] = RV(true),
                 watch: RV[Boolean] = RV(true))

def initState() = State()

def mounted(s: State, props: Props): Unit = {
  Api.get[TaskSharesDto](s"/api/tasks/${props.taskId}/shares").foreach(s.shares := _)
  Api.get[HelpdeskSettingsDto](s"/api/projects/${props.projectId}/helpdesk").foreach(x => s.settings := Some(x))
}

def save(s: State, props: Props, groups: List[UUID], users: List[UUID]): Unit =
  Toasts.report(Api.put[TaskSharesDto](s"/api/tasks/${props.taskId}/shares", TaskSharesReq(groups, users, s.notifyNew.v, Some(s.watch.v)).asJson)) { r =>
    s.shares := r
    Toasts.show(if (s.notifyNew.v) "共有先を変えました（新しく見える人に通知しました）" else "共有先を変えました")
    Store.changed()
    props.onChanged(())
  }

/** 選んだもの（"g:ID" はグループ、"u:ID" は人）を加える. */
def add(s: State, props: Props, v: String): Unit = {
  val cur = s.shares.v
  val gs = cur.groups.map(_.id)
  val us = cur.users.map(_.id)
  v.split(":", 2) match {
    case Array("g", id) => save(s, props, (gs :+ UUID.fromString(id)).distinct, us)
    case Array("u", id) => save(s, props, gs, (us :+ UUID.fromString(id)).distinct)
    case _              => ()
  }
}

def removeGroup(s: State, props: Props, id: UUID): Unit =
  save(s, props, s.shares.v.groups.map(_.id).filterNot(_ == id), s.shares.v.users.map(_.id))

def removeUser(s: State, props: Props, id: UUID): Unit =
  save(s, props, s.shares.v.groups.map(_.id), s.shares.v.users.map(_.id).filterNot(_ == id))

def options(s: State) = computed {
  val st = s.settings.v
  val cur = s.shares.v
  val gs = st.toList.flatMap(_.groups)
  HelpGroups.options(gs).filterNot(o => cur.groups.exists(_.id.toString == o.value)).map(o => o.copy(value = "g:" + o.value)) ++
    st.toList.flatMap(_.requesters).filterNot(u => cur.users.exists(_.id == u.id)).map(u => Opt("u:" + u.id, u.name, user = Some(person(u))))
}

def groupName(s: State, g: RequesterGroupDto): String = HelpGroups.path(s.settings.v.toList.flatMap(_.groups), g)
def empty(s: State) = s.shares.mapC(x => x.groups.isEmpty && x.users.isEmpty)
</script>

<template>
  <div class="field share"><span>共有（問い合わせ側）</span>
    <div v-for="g in s.shares.mapC(_.groups)" :key="g.id" class="row small">
      <span class="grow ell">👥 {{ groupName(s, g) }}</span>
      <button v-if="props.canEdit" class="ghost small" @click="removeGroup(s, props, g.id)" title="共有をやめる">×</button>
    </div>
    <div v-for="u in s.shares.mapC(_.users)" :key="u.id" class="row small">
      <Avatar :user="Some(person(u))" :size="20" /><span class="grow ell">{{ u.name }}</span>
      <button v-if="props.canEdit" class="ghost small" @click="removeUser(s, props, u.id)" title="共有をやめる">×</button>
    </div>
    <p v-if="empty(s)" class="muted small">起票者のほかには見えません</p>
    <template v-if="props.canEdit">
      <Combo :live="Some(options(s))" :small="true" :keep="false" placeholder="＋ グループ・ユーザに共有" :onChange="v => add(s, props, v)" />
      <label class="small notify"><input type="checkbox" :checked="s.notifyNew.v" @change="s.notifyNew := Dom.checked(event)"> 共有先に通知する</label>
      <label class="small notify"><input type="checkbox" :checked="s.watch.v" @change="s.watch := Dom.checked(event)"> ウォッチャーにも登録する</label>
    </template>
  </div>
</template>

<style scoped>
.notify { display: flex; align-items: center; gap: .3rem; margin-top: .3rem; }
</style>
