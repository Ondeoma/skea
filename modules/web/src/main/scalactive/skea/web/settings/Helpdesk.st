<script lang="scala">
import skea.web.*
import skea.web.parts.*
import io.circe.syntax.*
import org.scalajs.dom
import skea.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// ヘルプデスク: モードの切り替え、ゲスト登録（窓口の URL・メールのドメイン）、問い合わせする"ユーザ"のグループ（親子の階層）
case class State(data: RV[Option[HelpdeskSettingsDto]] = RV(None),
                 helpdesk: RV[Boolean] = RV(false),
                 guest: RV[Boolean] = RV(false),
                 domains: RV[String] = RV(""),
                 doneDays: RV[String] = RV("7"),
                 newName: RV[String] = RV(""),
                 newParent: RV[String] = RV(""),
                 newDomain: RV[String] = RV(""),
                 editDomain: RV[String] = RV(""),
                 editing: RV[Option[UUID]] = RV(None),
                 editName: RV[String] = RV(""),
                 editParent: RV[String] = RV(""))

def initState() = State()

def pid: UUID = Store.pid

def load(s: State): Unit =
  Api.get[HelpdeskSettingsDto](s"/api/projects/$pid/helpdesk").foreach { d =>
    s.data := Some(d)
    s.helpdesk := d.helpdesk
    s.guest := d.guestSignup
    s.domains := d.guestDomains.mkString(", ")
    s.doneDays := d.doneHideDays.toString
  }

def mounted(s: State): Unit = load(s)

def save(s: State): Unit = {
  val ds = s.domains.v.split("[,\\s]+").map(_.trim).filter(_.nonEmpty).toList
  Toasts.report(Api.putUnit(s"/api/projects/$pid/helpdesk", HelpdeskSettingsReq(s.helpdesk.v, s.guest.v, ds, s.doneDays.v.trim.toIntOption).asJson)) { _ =>
    Toasts.show("保存しました")
    Store.reloadMeta()
    load(s)
  }
}

def parentOf(v: String): Option[UUID] = Option(v).filter(_.nonEmpty).map(UUID.fromString)
def domainOf(v: String): Option[String] = Option(RequesterGroups.normalizeDomain(v)).filter(_.nonEmpty)

/** グループのドメインの表示（引き継いでいるときはそう書く）. */
def domainLabel(s: State, g: RequesterGroupDto): String =
  g.domain.filter(_.nonEmpty).map("@" + _).getOrElse(
    RequesterGroups.effectiveDomain(s.data.v.toList.flatMap(_.groups), g).fold("")(d => s"@$d（親から）"))

def addGroup(s: State): Unit = if (s.newName.v.trim.nonEmpty)
  Toasts.report(Api.post[io.circe.Json](s"/api/projects/$pid/requester-groups", RequesterGroupReq(s.newName.v.trim, parentOf(s.newParent.v), domainOf(s.newDomain.v)).asJson)) { _ =>
    s.newName := ""
    s.newDomain := ""
    load(s)
  }

def startEdit(s: State, g: RequesterGroupDto): Unit = {
  s.editing := Some(g.id)
  s.editName := g.name
  s.editParent := g.parentId.map(_.toString).getOrElse("")
  s.editDomain := g.domain.getOrElse("")
}

def saveGroup(s: State, g: RequesterGroupDto): Unit =
  Toasts.report(Api.putUnit(s"/api/projects/$pid/requester-groups/${g.id}", RequesterGroupReq(s.editName.v.trim, parentOf(s.editParent.v), domainOf(s.editDomain.v)).asJson)) { _ =>
    s.editing := None
    load(s)
  }

def removeGroup(s: State, g: RequesterGroupDto): Unit =
  Dialog.confirm(s"グループ「${g.name}」を削除しますか？（子のグループも削除され、このグループへの共有は外れます）", "削除する") {
    Toasts.report(Api.delete(s"/api/projects/$pid/requester-groups/${g.id}"))(_ => load(s))
  }

def addMember(s: State, g: RequesterGroupDto, uid: String): Unit = if (uid.nonEmpty)
  Toasts.report(Api.putUnit(s"/api/projects/$pid/requester-groups/${g.id}/members", GroupMemberReq(UUID.fromString(uid)).asJson))(_ => load(s))

def removeMember(s: State, g: RequesterGroupDto, u: UserBrief): Unit =
  Toasts.report(Api.delete(s"/api/projects/$pid/requester-groups/${g.id}/members/${u.id}"))(_ => load(s))

def groups(s: State) = s.data.mapC(_.toList.flatMap(_.groups))
def tree(s: State) = s.data.mapC(d => HelpGroups.tree(d.toList.flatMap(_.groups)))
def parentOpts(s: State) = groups(s).mapC(HelpGroups.options)
/** 自分自身と子孫は親に選べない（サーバーでも確かめる）. */
def editParentOpts(s: State, g: RequesterGroupDto) = groups(s).mapC { gs =>
  def desc(id: UUID): Set[UUID] = gs.filter(_.parentId.contains(id)).flatMap(c => desc(c.id) + c.id).toSet
  val ng = desc(g.id) + g.id
  HelpGroups.options(gs.filterNot(x => ng(x.id)))
}
def memberOpts(s: State, g: RequesterGroupDto) = s.data.mapC(d => Opts.users(d.toList.flatMap(_.requesters).filterNot(u => g.members.exists(_.id == u.id))))
def indent(depth: Int): String = s"padding-left:${depth * 1.4}rem"
def portalUrl: String = s"${dom.window.location.origin}/help/${Store.meta.v.map(_.project.key).getOrElse("")}"
def requesters(s: State) = s.data.mapC(_.toList.flatMap(_.requesters))
def isEditing(s: State, g: RequesterGroupDto) = s.editing.mapC(_.contains(g.id))
</script>

<template>
  <section>
    <h2>ヘルプデスク</h2>
    <p class="muted small">ヘルプデスクにすると、メンバーを問い合わせする「ユーザ」と、回答する「エージェント」（閲覧・編集・管理）に分けられます。
      ユーザには自分の問い合わせと共有されたタスクだけが簡素な画面で見え、エージェントはユーザに見えない内部メモを書けます。</p>
    <label class="check"><input type="checkbox" :checked="s.helpdesk.v" :disabled="!Store.isProjectAdmin.v" @change="s.helpdesk := Dom.checked(event)"> ヘルプデスクとして使う</label>
    <label class="check"><input type="checkbox" :checked="s.guest.v" :disabled="!Store.isProjectAdmin.v" @change="s.guest := Dom.checked(event)"> ゲスト登録を許可する（窓口から OIDC でログインした人が、自分でユーザとして参加できる）</label>
    <label class="field"><span>ゲスト登録できるメールアドレスのドメイン（カンマ区切り。空ならだれでも）</span>
      <input type="text" v-model="s.domains" placeholder="example.com, example.co.jp" :disabled="!Store.isProjectAdmin.v"></label>
    <p class="muted small">ヘルプデスクのプロジェクトは、すべてのタスクを1つのアクティブスプリントに置きます（スプリントは自動で作り、完了・削除はできません。バックログはありません）。</p>
    <label class="field"><span>ボードに完了したタスクを出す日数（完了してからこの日数を過ぎると、ボードに表示しなくなります。0 なら完了したらすぐ）</span>
      <input type="number" min="0" max="3650" v-model="s.doneDays" class="days" :disabled="!Store.isProjectAdmin.v"></label>
    <div class="field"><span>窓口の URL（ユーザに案内する）</span>
      <div class="row"><code class="grow">{{ portalUrl }}</code><button class="small" @click="Dom.copy(portalUrl)">コピー</button></div></div>
    <button v-if="Store.isProjectAdmin" class="primary" @click="save(s)">保存</button>

    <h3>ユーザのグループ</h3>
    <p class="muted small">タスクの画面の「共有」で、グループにタスクを見せて通知できます。親のグループに共有したタスクは、子のグループの人にも見えます。
      グループに入れられるのは「ユーザ（問い合わせ）」のメンバーです（メンバーと権限の画面で追加するか、ゲスト登録で参加します）。
      グループにメールのドメインを付けると、同じドメインのメールアドレスのユーザが、自分の問い合わせをそのグループに共有できます（空なら親のドメインを引き継ぎます）。
      共有したグループの人は、ウォッチャーにも登録します。</p>
    <div v-for="x in tree(s)" :key="x._1.id" class="group" :style="indent(x._2)">
      <div v-if="isEditing(s, x._1).mapC(!_)" class="row">
        <b>👥 {{ x._1.name }}</b><span class="muted small grow">{{ domainLabel(s, x._1) }}</span>
        <button v-if="Store.isProjectAdmin" class="ghost small" @click="startEdit(s, x._1)">変更</button>
        <button v-if="Store.isProjectAdmin" class="ghost small" @click="removeGroup(s, x._1)">削除</button>
      </div>
      <form v-if="isEditing(s, x._1)" class="row" @submit.prevent="saveGroup(s, x._1)">
        <input type="text" v-model="s.editName" class="grow">
        <Combo :bind="Some(s.editParent)" :live="Some(editParentOpts(s, x._1))" clearLabel="（親なし）" placeholder="親のグループ" :small="true" />
        <input type="text" v-model="s.editDomain" placeholder="ドメイン（空なら親から）" class="dom">
        <button type="submit" class="primary small">保存</button><button type="button" class="small" @click="s.editing := None">キャンセル</button>
      </form>
      <div class="members">
        <span v-for="u in x._1.members" :key="u.id" class="chip"><Avatar :user="Some(person(u))" :size="16" /> {{ u.name }}
          <button v-if="Store.isProjectAdmin" class="ghost small" @click="removeMember(s, x._1, u)" title="グループから外す">×</button></span>
        <Combo v-if="Store.isProjectAdmin" :live="Some(memberOpts(s, x._1))" :small="true" :keep="false" placeholder="＋ ユーザを追加"
          :onChange="v => addMember(s, x._1, v)" />
      </div>
    </div>
    <p v-if="tree(s).mapC(_.isEmpty)" class="muted small">グループはまだありません</p>
    <form v-if="Store.isProjectAdmin" class="row add" @submit.prevent="addGroup(s)">
      <input type="text" v-model="s.newName" placeholder="新しいグループの名前（例: A社 経理部）" class="grow">
      <Combo :bind="Some(s.newParent)" :live="Some(parentOpts(s))" clearLabel="（親なし）" placeholder="親のグループ" :small="true" />
      <input type="text" v-model="s.newDomain" placeholder="ドメイン（例: a-sha.example.com）" class="dom">
      <button type="submit">追加</button>
    </form>

    <h3>ユーザ（{{ requesters(s).mapC(_.size) }}人）</h3>
    <div class="members">
      <span v-for="u in requesters(s)" :key="u.id" class="chip"><Avatar :user="Some(person(u))" :size="16" /> {{ u.name }}</span>
    </div>
  </section>
</template>

<style scoped>
.days { width: 8rem; }
.check { display: flex; align-items: center; gap: .4rem; margin: .5rem 0; }
.group { border-left: 2px solid var(--line); margin: .5rem 0; padding: .3rem .6rem; }
.members { display: flex; flex-wrap: wrap; gap: .3rem; align-items: center; margin-top: .3rem; }
.chip { display: inline-flex; align-items: center; gap: .25rem; }
.add { margin-top: .8rem; }
.dom { width: 14rem; max-width: 100%; }
code { overflow-wrap: anywhere; }
</style>
