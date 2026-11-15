<script lang="scala">
import skea.web.*
import skea.web.parts.*
import io.circe.Json
import io.circe.syntax.*
import skea.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// Webhook（送信）: イベントを JSON で POST する。X-Skea-Signature: sha256=<HMAC> で検証できる
case class State(hooks: RV[List[WebhookDto]] = RV(Nil),
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

def mounted(s: State): Unit = load(s)

def edit(s: State, w: Option[WebhookDto]): Unit = {
  s.name := w.map(_.name).getOrElse(""); s.url := w.map(_.url).getOrElse("https://")
  s.events := w.map(_.events).getOrElse(Nil); s.enabled := w.forall(_.enabled); s.editing := Some(w)
}

def save(s: State): Unit = {
  val r = WebhookReq(s.name.v, s.url.v, s.events.v, s.enabled.v)
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
def eventsLabel(w: WebhookDto): String = if (w.events.isEmpty) "すべて" else w.events.map(EventKind.label).mkString(", ")
</script>

<template>
  <section>
    <div class="row"><h2>Webhook</h2><span class="grow"></span>
      <button class="small" @click="showSample(s, EventKind.TaskUpdated)">{ } 送信内容の見本</button>
      <button v-if="Store.isProjectAdmin" class="primary small" @click="edit(s, None)">＋ 追加</button></div>
    <p class="muted small">選んだイベントが起きると、JSON（event・project・task・actor・detail）を POST します。
      本文の HMAC-SHA256（秘密の値で計算）を <code>X-Skea-Signature: sha256=…</code> に付けます。失敗したら間隔を空けて再送します。</p>
    <div v-for="w in s.hooks" :key="w.id" class="panel">
      <div class="row"><b>{{ w.name }}</b><span v-if="!w.enabled" class="badge">停止中</span><span class="grow"></span>
        <button class="small" @click="test(w)">テスト送信</button><button class="small" @click="showDeliveries(s, w)">送信履歴</button>
        <button v-if="Store.isProjectAdmin" class="ghost small" @click="edit(s, Some(w))">編集</button><button v-if="Store.isProjectAdmin" class="ghost small" @click="remove(s, w)">削除</button></div>
      <div class="small"><code>{{ w.url }}</code></div>
      <div class="small muted">イベント: {{ eventsLabel(w) }}</div>
      <div class="small muted">秘密の値: <code>{{ w.secret }}</code></div>
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
          <h2>Webhook</h2>
          <label class="field"><span>名前</span><input type="text" v-model="s.name"></label>
          <label class="field"><span>送信先 URL</span><input type="url" v-model="s.url"></label>
          <div class="field"><span>イベント（選ばなければすべて）</span>
            <div class="chips"><label v-for="e in EventKind.all" class="chip"><input type="checkbox" :value="e" v-model="s.events"> {{ EventKind.label(e) }}</label></div>
          </div>
          <label class="small"><input type="checkbox" v-model="s.enabled"> 有効</label>
          <div class="actions"><button type="button" @click="s.editing := None">キャンセル</button><button type="submit" class="primary">保存</button></div>
        </form>
    </Modal>
  </section>
</template>

<style scoped>
.deliv { margin-top: .6rem; }
.json { background: var(--side); border: 1px solid var(--line); border-radius: 8px; padding: .6rem .8rem; font-size: .78rem; max-height: 50vh;
  overflow: auto; white-space: pre; margin: .3rem 0 .6rem; }
</style>
