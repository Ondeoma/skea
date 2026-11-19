<script lang="scala">
import skea.web.*
import skea.web.parts.*
import io.circe.Json
import io.circe.syntax.*
import skea.shared.*

import org.scalajs.dom
import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// Webhook（送信）: イベントを JSON で POST する。X-Skea-Signature: sha256=<HMAC> で検証できる
// stui のチャンネル: 保存した人の stui との連携（OAuth）で、イベントを文にしてチャンネルに投稿する
case class State(hooks: RV[List[WebhookDto]] = RV(Nil),
                 kind: RV[String] = RV(WebhookKind.Url),
                 stui: RV[Option[LinkServiceDto]] = RV(None),
                 channels: RV[List[StuiChannelDto]] = RV(Nil),
                 channelId: RV[String] = RV(""),
                 channelError: RV[String] = RV(""),
                 useHook: RV[Boolean] = RV(true),
                 editing: RV[Option[Option[WebhookDto]]] = RV(None),
                 name: RV[String] = RV(""),
                 url: RV[String] = RV(""),
                 events: RV[List[String]] = RV(Nil),
                 enabled: RV[Boolean] = RV(true),
                 deliveries: RV[List[DeliveryDto]] = RV(Nil),
                 shown: RV[Option[UUID]] = RV(None),
                 sample: RV[Option[String]] = RV(None),
                 sampleJson: RV[String] = RV(""),
                 payload: RV[Option[UUID]] = RV(None))

def initState() = State()


def load(s: State): Unit = Api.get[List[WebhookDto]](s"/api/projects/${Store.pid}/webhooks").foreach(s.hooks := _)

/** stui との連携（skea に設定されていて、自分が連携しているか）を読む. stui から戻ったときは、stui の通知の追加を開く. */
def mounted(s: State): Unit = {
  load(s)
  Api.get[List[LinkServiceDto]]("/api/integrations/links").foreach { ls =>
    s.stui := ls.find(_.provider == "stui")
    val q = dom.window.location.search
    if (q.contains("integration=stui") && Store.isProjectAdmin.v) {
      dom.window.history.replaceState(null, "", dom.window.location.pathname)
      if (q.contains("result=connected")) edit(s, None, WebhookKind.Stui)
      else Store.toast(s"${AppNames.nameOf("stui")} と連携できませんでした", error = true)
    }
  }
}

def edit(s: State, w: Option[WebhookDto], kind: String = WebhookKind.Url): Unit = {
  s.kind := w.map(x => if (WebhookKind.isStui(x.kind)) WebhookKind.Stui else x.kind).getOrElse(kind)
  s.useHook := w.forall(_.kind == WebhookKind.StuiHook)
  s.name := w.map(_.name).getOrElse(""); s.url := w.map(_.url).filter(_.nonEmpty).getOrElse("https://")
  s.events := w.map(_.events).getOrElse(Nil); s.enabled := w.forall(_.enabled)
  s.channelId := w.flatMap(_.channelId).getOrElse("")
  s.editing := Some(w)
  if (s.kind.v == WebhookKind.Stui) loadChannels(s)
}

def setKind(s: State, k: String): Unit = { s.kind := k; if (k == WebhookKind.Stui) loadChannels(s) }

/** 自分が参加している stui のチャンネル（連携していなければ空で、連携のボタンを出す）. */
def loadChannels(s: State): Unit = {
  s.channelError := ""
  if (s.stui.v.exists(_.connected))
    Api.get[List[StuiChannelDto]]("/api/integrations/stui/channels").onComplete {
      case scala.util.Success(cs) => s.channels := cs
      case scala.util.Failure(ApiError(409, _)) => s.stui := s.stui.v.map(_.copy(connected = false))
      case scala.util.Failure(e) => s.channelError := e.getMessage
    }
}

/** stui の許可画面へ（戻ってきたら、この画面で stui の通知の追加を開く）. */
def connectStui(): Unit =
  dom.window.location.href = "/api/integrations/stui/connect?returnTo=" + Api.enc(dom.window.location.pathname)

def stuiReady(s: State) = s.stui.mapC(_.exists(_.connected))
def isStui(s: State) = s.kind.mapC(_ == WebhookKind.Stui)
def channelOpts(s: State) = s.channels.mapC(_.map(c => c.id -> ((if (c.kind == "private") "🔒 " else "# ") + c.name + (if (c.admin) "（管理者）" else ""))))
/** 選んだチャンネルの管理者なら、受信 Webhook を作って送る選択肢を出す. */
def canHook(s: State) = computed(s.channels.v.find(_.id == s.channelId.v).exists(_.admin))

def save(s: State): Unit = {
  val ch = s.channels.v.find(_.id == s.channelId.v)
  val r =
    if (s.kind.v == WebhookKind.Stui)
      WebhookReq(s.name.v, "", s.events.v, s.enabled.v, Some(if (ch.exists(_.admin) && s.useHook.v) WebhookKind.StuiHook else WebhookKind.Stui),
        Option(s.channelId.v).filter(_.nonEmpty), ch.map(_.name))
    else WebhookReq(s.name.v, s.url.v, s.events.v, s.enabled.v, Some(WebhookKind.Url))
  val f = s.editing.v.flatten match {
    case Some(w) => Api.putUnit(s"/api/webhooks/${w.id}", r.asJson)
    case None    => Api.post[WebhookDto](s"/api/projects/${Store.pid}/webhooks", r.asJson).map(_ => ())
  }
  Store.report(f) { _ => s.editing := None; load(s) }
}

def remove(s: State, w: WebhookDto): Unit =
  Dialog.confirm(s"Webhook「${w.name}」を削除しますか？") { Store.report(Api.delete(s"/api/webhooks/${w.id}"))(_ => load(s)) }

def test(w: WebhookDto): Unit = Store.report(Api.post[Json](s"/api/webhooks/${w.id}/test")) { r =>
  val ok = r.hcursor.get[Boolean]("ok").getOrElse(false)
  Store.toast(if (ok) s"送信できました（HTTP ${r.hcursor.get[Int]("statusCode").getOrElse(0)}）" else s"送信できませんでした: ${r.hcursor.get[String]("result").getOrElse("")}", error = !ok)
}

def showDeliveries(s: State, w: WebhookDto): Unit =
  if (s.shown.v.contains(w.id)) s.shown := None
  else Api.get[List[DeliveryDto]](s"/api/webhooks/${w.id}/deliveries").foreach { d => s.deliveries := d; s.shown := Some(w.id) }

/** 送信する JSON の見本（イベントを選ぶ）. */
def showSample(s: State, event: String): Unit =
  Api.get[Json](s"/api/projects/${Store.pid}/webhooks/sample?event=${Api.enc(event)}").foreach { j =>
    s.sampleJson := j.spaces2
    s.sample := Some(event)
  }

def togglePayload(s: State, d: DeliveryDto): Unit = s.payload := (if (s.payload.v.contains(d.id)) None else Some(d.id))
def isPayload(s: State, d: DeliveryDto) = s.payload.mapC(_.contains(d.id))
val eventOpts: List[Opt] = EventKind.all.map(e => Opt(e, EventKind.label(e), hint = e))
def sampleEvent(s: State) = s.sample.mapC(_.getOrElse(""))
def headersSample(s: State) = s.sample.mapC(e => List(
  "POST <送信先 URL>",
  "Content-Type: application/json",
  "X-Skea-Event: " + e.getOrElse(""),
  "X-Skea-Delivery: <送信ごとの ID（再送でも同じ）>",
  "X-Skea-Signature: sha256=<本文の HMAC-SHA256（秘密の値で計算）>",
  "User-Agent: skea-webhook").mkString("\n"))

def isShown(s: State, w: WebhookDto) = s.shown.mapC(_.contains(w.id))
def postedBy(w: WebhookDto): String =
  if (w.kind == WebhookKind.StuiHook) s"stui の受信 Webhook で送ります（${w.owner.fold("")(_.name + " が")}作成）"
  else w.owner.fold("設定した人がいません（設定し直してください）")(u => s"${u.name} の ${AppNames.nameOf("stui")} のアカウントで投稿します")
def eventsLabel(w: WebhookDto): String = if (w.events.isEmpty) "すべて" else w.events.map(EventKind.label).mkString(", ")
</script>

<template>
  <section>
    <div class="row"><h2>Webhook</h2><span class="grow"></span>
      <button class="small" @click="showSample(s, EventKind.TaskUpdated)">{ } 送信内容の見本</button>
      <button v-if="Store.isProjectAdmin.v && s.stui.v.isDefined" class="small" @click="edit(s, None, WebhookKind.Stui)">💬 {{ AppNames.of("stui") }} に通知</button>
      <button v-if="Store.isProjectAdmin" class="primary small" @click="edit(s, None)">＋ Webhook を追加</button></div>
    <p class="muted small">選んだイベントが起きると、JSON（event・project・task・actor・detail）を POST します。
      本文の HMAC-SHA256（秘密の値で計算）を <code>X-Skea-Signature: sha256=…</code> に付けます。失敗したら間隔を空けて再送します。</p>
    <p v-if="s.stui.mapC(_.isDefined)" class="muted small">「{{ AppNames.of("stui") }} に通知」は、URL や秘密の値を用意しなくても、{{ AppNames.of("stui") }} のチャンネルにイベントを文で投稿します（あなたの {{ AppNames.of("stui") }} との連携を使います）。</p>
    <div v-for="w in s.hooks" :key="w.id" class="panel">
      <div class="row"><b>{{ w.name }}</b><span v-if="WebhookKind.isStui(w.kind)" class="badge">{{ AppNames.of("stui") }}</span><span v-if="!w.enabled" class="badge">停止中</span><span class="grow"></span>
        <button class="small" @click="test(w)">テスト送信</button><button class="small" @click="showDeliveries(s, w)">送信履歴</button>
        <button v-if="Store.isProjectAdmin" class="ghost small" @click="edit(s, Some(w))">編集</button><button v-if="Store.isProjectAdmin" class="ghost small" @click="remove(s, w)">削除</button></div>
      <template v-if="WebhookKind.isStui(w.kind)">
        <div class="small">💬 チャンネル <b>#{{ w.channelName.getOrElse(w.channelId.getOrElse("")) }}</b></div>
        <div class="small muted">イベント: {{ eventsLabel(w) }}</div>
        <div class="small muted">{{ postedBy(w) }}</div>
      </template>
      <template v-else>
        <div class="small"><code>{{ w.url }}</code></div>
        <div class="small muted">イベント: {{ eventsLabel(w) }}</div>
        <div class="small muted">秘密の値: <code>{{ w.secret }}</code></div>
      </template>
      <div v-if="isShown(s, w)" class="table-wrap deliv">
        <table class="list"><tbody>
          <template v-for="d in s.deliveries">
            <tr class="clickable" @click="togglePayload(s, d)" title="クリックで送った本文を表示">
              <td class="small nowrap">{{ Fmt.dateTime(d.createdAt) }}</td><td class="small">{{ EventKind.label(d.event) }}</td>
              <td class="small">{{ d.statusCode.map(_.toString).getOrElse("-") }}</td>
              <td class="small">{{ if (d.doneAt.isDefined) "送信済み" else if (d.failed) "失敗" else s"再送待ち（${d.attempts}回）" }}</td>
              <td class="small muted">{{ d.lastError.getOrElse("") }}</td>
            </tr>
            <tr v-if="isPayload(s, d)"><td colspan="5"><pre class="json">{{ d.payload.spaces2 }}</pre></td></tr>
          </template>
        </tbody></table>
      </div>
    </div>
    <p v-if="s.hooks.mapC(_.isEmpty)" class="muted">Webhook はありません</p>
    <Modal v-if="s.sample.mapC(_.isDefined)" :wide="true" :onClose="_ => s.sample := None">
      <h2>送信内容の見本</h2>
      <p class="muted small">イベントごとに送る JSON の例です（値は架空のもの）。task はイベントの時点のタスク、detail はイベントごとの詳細です。</p>
      <div class="row">
        <Combo :options="eventOpts" :shows="Some(sampleEvent(s))" :onChange="v => showSample(s, v)" />
        <span class="grow"></span>
        <button class="small" @click='Dom.copy(s.sampleJson.v, "JSON をコピーしました")'>JSON をコピー</button>
      </div>
      <h3>ヘッダー</h3>
      <pre class="json">{{ headersSample(s) }}</pre>
      <h3>本文</h3>
      <pre class="json">{{ s.sampleJson }}</pre>
      <p class="muted small">署名の検証: 受け取った本文（バイト列）を秘密の値で HMAC-SHA256 し、16 進数にして <code>X-Skea-Signature</code> の <code>sha256=</code> の後ろと比べます。</p>
      <div class="actions"><button @click="s.sample := None">閉じる</button></div>
    </Modal>
    <Modal v-if="s.editing.mapC(_.isDefined)" :onClose="_ => s.editing := None">
      <form @submit.prevent="save(s)">
          <h2>{{ if (isStui(s).v) AppNames.nameOf("stui") + " に通知" else "Webhook" }}</h2>
          <div v-if="s.stui.mapC(_.isDefined)" class="field"><span>送り方</span>
            <div class="chips">
              <button type="button" :class='if (isStui(s).v) "chip" else "chip active"' @click="setKind(s, WebhookKind.Url)">Webhook（URL に JSON を送る）</button>
              <button type="button" :class='if (isStui(s).v) "chip active" else "chip"' @click="setKind(s, WebhookKind.Stui)">💬 {{ AppNames.of("stui") }} のチャンネルに投稿</button>
            </div>
          </div>
          <template v-if="isStui(s).v">
            <div v-if="!stuiReady(s).v" class="panel">
              <p class="small">{{ AppNames.of("stui") }} のチャンネルに投稿するには、あなたの {{ AppNames.of("stui") }} のアカウントと連携します（{{ AppNames.of("stui") }} の画面で許可します）。</p>
              <button type="button" class="primary small" @click="connectStui()">{{ AppNames.of("stui") }} と連携する</button>
            </div>
            <template v-else>
              <label class="field"><span>チャンネル（あなたが参加しているもの）</span>
                <select v-model="s.channelId">
                  <option value="">（選ぶ）</option>
                  <option v-for="o in channelOpts(s)" :value="o._1">{{ o._2 }}</option>
                </select>
              </label>
              <p v-if="s.channelError.mapC(_.nonEmpty)" class="error small">{{ s.channelError }}</p>
              <div v-if="canHook(s).v" class="field"><span>送り方（あなたはこのチャンネルの管理者です）</span>
                <label class="small"><input type="radio" name="via" :checked="s.useHook.v" @change="s.useHook := true">
                  {{ AppNames.of("stui") }} に受信 Webhook を作って送る（おすすめ. あなたが連携を外したりチャンネルを抜けたりしても届きます）</label>
                <label class="small"><input type="radio" name="via" :checked="!s.useHook.v" @change="s.useHook := false">
                  あなたの {{ AppNames.of("stui") }} のアカウントの連携で投稿する</label>
              </div>
              <label class="field"><span>名前（任意. 空ならチャンネル名）</span><input type="text" v-model="s.name"></label>
              <p v-if="canHook(s).v && s.useHook.v" class="muted small">保存すると、このチャンネルに「{{ AppNames.self }}」の受信 Webhook を作り、そこに送ります（{{ AppNames.of("stui") }} のチャンネルの設定にも出ます）。</p>
              <p v-else class="muted small">保存すると、あなたの {{ AppNames.of("stui") }} のアカウントの連携で、{{ AppNames.of("stui") }} のボット「{{ AppNames.self }}」としてチャンネルに投稿します。</p>
            </template>
          </template>
          <template v-else>
            <label class="field"><span>名前</span><input type="text" v-model="s.name"></label>
            <label class="field"><span>送信先 URL</span><input type="url" v-model="s.url"></label>
          </template>
          <div class="field"><span>イベント（選ばなければすべて）</span>
            <div class="chips"><label v-for="e in EventKind.all" class="chip"><input type="checkbox" :value="e" v-model="s.events"> {{ EventKind.label(e) }}</label></div>
          </div>
          <label class="small"><input type="checkbox" v-model="s.enabled"> 有効</label>
          <div class="actions"><button type="button" @click="s.editing := None">キャンセル</button><button type="submit" class="primary" :disabled="isStui(s).v && !stuiReady(s).v">保存</button></div>
        </form>
    </Modal>
  </section>
</template>

<style scoped>
.deliv { margin-top: .6rem; }
.json { background: var(--side); border: 1px solid var(--line); border-radius: 8px; padding: .6rem .8rem; font-size: .78rem; max-height: 50vh;
  overflow: auto; white-space: pre; margin: .3rem 0 .6rem; }
</style>
