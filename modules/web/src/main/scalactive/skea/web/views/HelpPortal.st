<script lang="scala">
import skea.web.*
import skea.web.parts.*
import org.scalajs.dom
import skea.shared.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.util.{Failure, Success}

// ヘルプデスクの窓口（/help/KEY）: 新しい問い合わせと、自分に見える問い合わせの検索（parts/HelpSearchPanel）.
// ログインしていない人・まだ参加していない人には案内を出し、ゲスト登録を許可していれば OIDC でログインして参加できる.
case class Props(key: String)

case class State(portal: RV[Option[HelpPortalDto]] = RV(None),
                 missing: RV[Boolean] = RV(false),
                 asking: RV[Boolean] = RV(false),
                 title: RV[String] = RV(""),
                 editor: RV[Option[EditorHandle]] = RV(None),
                 busy: RV[Boolean] = RV(false),
                 shareOpts: RV[List[HelpShareItemDto]] = RV(Nil),
                 shareSel: RV[List[java.util.UUID]] = RV(Nil),
                 form: RV[List[CustomFieldDto]] = RV(Nil),
                 // フォームの項目の値. 画面の描き直しのきっかけにしない（入力のたびに入力欄が作り直されてフォーカスが外れるため）.
                 // 値は入力欄（FieldInput）自身が持っていて、ここは送信に使うだけ
                 values: scala.collection.mutable.Map[String, io.circe.Json] = scala.collection.mutable.Map.empty,
                 error: RV[String] = RV(""))

def initState() = State()

def load(s: State, props: Props): Unit =
  Api.portal(props.key).foreach {
    case None => s.missing := true
    case Some(p) =>
      s.portal := Some(p)
      pageTitle(p.name)
      if (p.role.isDefined) cleanUrl(props)
      // ログイン・登録して戻ってきたら、そのまま参加する
      else if (Store.me.v.isDefined && p.guestSignup && joinRequested) join(s, props)
  }

def joinRequested: Boolean = new dom.URL(dom.window.location.href).searchParams.get("join") == "1"

/** ログインの戻り先に付けた ?join=1 を外す（元の窓口の URL にする）. */
def cleanUrl(props: Props): Unit =
  if (joinRequested) dom.window.history.replaceState(null, "", s"/help/${props.key}")

def mounted(s: State, props: Props): Unit = {
  load(s, props)
}

/** ログイン（初めての人は IdP での登録）して、この窓口に戻ってくる. join なら戻ったときに参加する. */
def login(props: Props, join: Boolean): Unit =
  dom.window.location.href = "/auth/login?returnTo=" + js.URIUtils.encodeURIComponent(s"/help/${props.key}" + (if (join) "?join=1" else ""))

def join(s: State, props: Props): Unit =
  Api.joinPortal(props.key).onComplete {
    case Success(p) =>
      s.portal := Some(p)
      cleanUrl(props)
      Store.loadAll()
      Store.toast("窓口に登録しました")
    case Failure(e) =>
      cleanUrl(props)
      s.error := e.getMessage
  }

def submit(s: State): Unit = (s.portal.v, s.editor.v) match {
  case (Some(p), Some(ed)) if s.title.v.trim.nonEmpty && !s.busy.v =>
    s.busy := true
    val desc = if (ed.isEmpty()) None else Some(Editor.fromJs(ed.getJSON()))
    Api.createRequest(p.projectId, HelpRequestReq(s.title.v.trim, desc, Option(s.shareSel.v).filter(_.nonEmpty), Option(s.values.toMap).filter(_.nonEmpty))).onComplete {
      case Success(t) =>
        s.busy := false
        s.asking := false
        s.title := ""
        s.shareSel := Nil
        s.values.clear()
        Store.toast(s"${t.key} で受け付けました")
        Router.go(s"/help/t/${t.key}")
      case Failure(e) =>
        s.busy := false
        Store.error(e)
    }
  case (_, _) if s.title.v.trim.isEmpty => Store.toast("件名を入力してください", error = true)
  case _ => ()
}

/** 問い合わせの入力欄を開く（共有できるグループ＝自分のメールアドレスと同じドメインのものを読む）. */
def ask(s: State): Unit = {
  s.values.clear()
  s.asking := true
  s.portal.v.foreach { p =>
    Api.get[List[HelpShareItemDto]](s"/api/help/projects/${p.projectId}/share-options").foreach(s.shareOpts := _)
    Api.get[List[CustomFieldDto]](s"/api/help/projects/${p.projectId}/form").foreach(s.form := _)
  }
}

/** フォームの項目の値（空なら消す）. */
def setValue(s: State, f: CustomFieldDto, v: io.circe.Json): Unit =
  if (v.isNull) s.values -= f.id.toString else s.values(f.id.toString) = v

def toggleShare(s: State, id: java.util.UUID): Unit =
  s.shareSel := (if (s.shareSel.v.contains(id)) s.shareSel.v.filterNot(_ == id) else s.shareSel.v :+ id)
def shareClass(s: State, id: java.util.UUID) = s.shareSel.mapC(l => if (l.contains(id)) "chip active" else "chip")

def member(s: State) = s.portal.mapC(_.exists(_.role.isDefined))
def guest(s: State) = s.portal.mapC(_.exists(p => p.role.isEmpty))
def loggedIn = Store.me.mapC(_.isDefined)
def agent(s: State) = s.portal.mapC(_.exists(_.role.exists(r => Roles.rank(r) >= 1)))
def name(s: State) = s.portal.mapC(_.map(_.name).getOrElse(""))
def desc(s: State) = s.portal.mapC(_.map(_.description).getOrElse(""))
</script>

<template>
  <div class="page narrow help">
    <!-- ゲスト登録を許可していない窓口は、ログインするまで名前も出さない（ログインすれば参加している人は使える） -->
    <div v-if="computed { s.missing.v && !loggedIn.v }" class="card welcome">
      <p>この窓口を使うにはログインしてください。</p>
      <button class="primary" @click="login(props, false)">ログイン</button>
    </div>
    <p v-if="computed { s.missing.v && loggedIn.v }" class="muted">窓口が見つかりません。URL を確かめるか、窓口の担当者に参加を依頼してください。</p>

    <template v-if="s.portal.mapC(_.isDefined)">
      <h1>{{ name(s) }}</h1>
      <p v-if="desc(s).mapC(_.nonEmpty)" class="muted pre">{{ desc(s) }}</p>

      <!-- まだ参加していない（ゲスト登録） -->
      <div v-if="guest(s)" class="card welcome">
        <p v-if="loggedIn.mapC(!_)">この窓口から問い合わせるには、ログインしてください。初めての方は、ログインの画面から登録できます。ログインしたあとは、この画面に戻ります。</p>
        <p v-if="loggedIn">この窓口から問い合わせるには、登録してください。</p>
        <button v-if="loggedIn.mapC(!_)" class="primary" @click="login(props, true)">ログイン・新規登録</button>
        <button v-if="loggedIn" class="primary" @click="join(s, props)">この窓口に登録する</button>
        <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
      </div>

      <template v-if="member(s)">
        <p v-if="agent(s)" class="muted small">エージェントとして表示しています（問い合わせした人と同じ画面です）。<RouterLink :to='"/p/" + props.key + "/board"'>ボードへ →</RouterLink></p>
        <div v-if="s.asking.mapC(!_)" class="row">
          <button class="primary" @click="ask(s)">＋ 新しい問い合わせ</button>
        </div>
        <form v-if="s.asking" class="card ask" @submit.prevent="submit(s)">
          <h2>新しい問い合わせ</h2>
          <label class="field"><span>件名</span><input type="text" v-model="s.title" placeholder="例: ログインできません" autofocus></label>
          <div class="field"><span>内容</span>
            <RichEditor placeholder="状況をできるだけ詳しく書いてください（画像・ファイルはドロップか貼り付けで添付できます）"
              :uploadUrl='s.portal.v.map(p => "/api/help/projects/" + p.projectId + "/uploads")' :onReady="h => s.editor := Some(h)" :onSubmit="_ => submit(s)" />
          </div>
          <div v-for="f in s.form" :key="f.id" class="field">
            <span>{{ f.name }}<b v-if="f.required" class="req"> *</b></span>
            <FieldInput :field="f" :value="None" :live="true" :onChange="v => setValue(s, f, v)" />
            <span v-if="f.helpHint.nonEmpty" class="muted small hint">{{ f.helpHint }}</span>
          </div>
          <div v-if="s.shareOpts.mapC(_.nonEmpty)" class="field"><span>共有先（任意。選んだグループの人もこの問い合わせを見られ、更新の通知が届きます）</span>
            <div class="chips">
              <button v-for="o in s.shareOpts" :key="o.groupId" type="button" :class="shareClass(s, o.groupId)" @click="toggleShare(s, o.groupId)">👥 {{ o.name }}</button>
            </div>
          </div>
          <div class="row actions"><span class="grow"></span>
            <button type="button" @click="s.asking := false">キャンセル</button>
            <button type="submit" class="primary" :disabled="s.busy">送信する</button>
          </div>
        </form>

        <template v-for="p in s.portal.mapC(_.toList)">
          <HelpSearchPanel :projectId="p.projectId" :projectKey="p.key" />
        </template>
      </template>
    </template>
  </div>
</template>

<style scoped>
.help h1 { margin-bottom: .3rem; }
.pre { white-space: pre-wrap; }
.welcome { padding: 1.2rem; margin: 1rem 0; }
.ask { padding: 1rem; margin: 1rem 0; }
.actions { margin-top: .6rem; gap: .5rem; }
.chips { display: flex; flex-wrap: wrap; gap: .35rem; }
.req { color: var(--danger); }
.hint { margin-top: .15rem; }
</style>
