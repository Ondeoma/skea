<script lang="scala">
import skea.web.*
import skea.web.parts.*
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import skea.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

// 自動化のルール: トリガー（イベント・定期・手動・受信 Webhook）＋ 条件 ＋ アクション（条件分岐・Web リクエストを含む）
case class State(rules: RV[List[RuleDto]] = RV(Nil),
                 runs: RV[List[RunDto]] = RV(Nil),
                 editing: RV[Option[Option[RuleDto]]] = RV(None),
                 name: RV[String] = RV(""),
                 enabled: RV[Boolean] = RV(true),
                 ttype: RV[String] = RV("task_created"),
                 from: RV[String] = RV(""),
                 to: RV[String] = RV(""),
                 fields: RV[String] = RV(""),
                 time: RV[String] = RV("09:00"),
                 interval: RV[String] = RV(""),
                 query: RV[String] = RV(""),
                 condition: RV[String] = RV(""),
                 actions: RV[String] = RV("[]"),
                 runAs: RV[String] = RV(""),
                 error: RV[String] = RV(""),
                 help: RV[Boolean] = RV(false))

def initState() = State()


def load(s: State): Unit = {
  Api.get[List[RuleDto]](s"/api/projects/${Store.pid}/automations").foreach(s.rules := _)
  Api.get[List[RunDto]](s"/api/projects/${Store.pid}/automation-runs?limit=50").foreach(s.runs := _)
}

def mounted(s: State): Unit = load(s)

val triggerTypes: List[(String, String)] = List(
  "task_created" -> "タスクが作成されたとき",
  "task_updated" -> "タスクが更新されたとき",
  "status_changed" -> "ステータスが変わったとき",
  "comment_added" -> "コメントが追加されたとき",
  "worklog_added" -> "作業時間が記録されたとき",
  "commit_linked" -> "コミットがリンクされたとき",
  "sprint_started" -> "スプリントが開始されたとき",
  "sprint_completed" -> "スプリントが完了したとき",
  "scheduled" -> "定期的に（毎日・一定間隔）",
  "manual" -> "手動（タスク画面のボタン）",
  "incoming_webhook" -> "外部から Webhook を受け取ったとき"
)
def triggerLabel(t: String): String = triggerTypes.find(_._1 == t).map(_._2).getOrElse(t)

private def csv(s: String): Json = s.split(',').map(_.trim).filter(_.nonEmpty).toList.asJson
private def pretty(j: Json): String = j.spaces2

def edit(s: State, r: Option[RuleDto]): Unit = {
  val t = r.map(_.trigger).getOrElse(Json.obj("type" -> "task_created".asJson)).hcursor
  def str(k: String) = t.get[List[String]](k).map(_.mkString(", ")).orElse(t.get[String](k)).getOrElse("")
  s.name := r.map(_.name).getOrElse("")
  s.enabled := r.forall(_.enabled)
  s.ttype := t.get[String]("type").getOrElse("task_created")
  s.from := str("from"); s.to := str("to"); s.fields := str("fields")
  s.time := t.get[String]("time").getOrElse(""); s.interval := t.get[Int]("intervalMinutes").map(_.toString).getOrElse("")
  s.query := t.get[String]("query").getOrElse("")
  s.condition := t.downField("condition").focus.filterNot(_.isNull).map(pretty).getOrElse("")
  s.actions := r.map(x => pretty(x.actions)).getOrElse("[\n  {\"type\": \"comment\", \"text\": \"自動化: {{task.key}}\"}\n]")
  s.runAs := r.flatMap(_.runAs).map(_.id.toString).getOrElse("")
  s.error := ""
  s.editing := Some(r)
}

def trigger(s: State): Either[String, Json] = {
  val base = s.ttype.v match {
    case "status_changed"   => Json.obj("from" -> csv(s.from.v), "to" -> csv(s.to.v))
    case "task_updated"     => Json.obj("fields" -> csv(s.fields.v))
    case "scheduled"        => Json.obj("time" -> Option(s.time.v).filter(_.nonEmpty).asJson,
                                 "intervalMinutes" -> s.interval.v.toIntOption.asJson, "query" -> Option(s.query.v).filter(_.nonEmpty).asJson)
    case "incoming_webhook" => Json.obj("query" -> Option(s.query.v).filter(_.nonEmpty).asJson)
    case _                  => Json.obj()
  }
  val cond = if (s.condition.v.trim.isEmpty) Right(Json.Null) else parse(s.condition.v).left.map(e => s"条件の JSON が正しくありません: ${e.message}")
  cond.map(c => Json.obj("type" -> s.ttype.v.asJson).deepMerge(base).deepMerge(Json.obj("condition" -> c)).dropNullValues)
}

def save(s: State): Unit = {
  s.error := ""
  (trigger(s), parse(s.actions.v).left.map(e => s"アクションの JSON が正しくありません: ${e.message}")) match {
    case (Left(e), _) => s.error := e
    case (_, Left(e)) => s.error := e
    case (Right(t), Right(a)) =>
      val r = RuleReq(s.name.v, s.enabled.v, t, a, Option(s.runAs.v).filter(_.nonEmpty).map(UUID.fromString))
      val f = s.editing.v.flatten match {
        case Some(x) => Api.putUnit(s"/api/automations/${x.id}", r.asJson)
        case None    => Api.post[RuleDto](s"/api/projects/${Store.pid}/automations", r.asJson).map(_ => ())
      }
      f.onComplete {
        case Success(_) => s.editing := None; load(s)
        case Failure(e) => s.error := e.getMessage
      }
  }
}

def remove(s: State, r: RuleDto): Unit =
  Dialog.confirm(s"ルール「${r.name}」を削除しますか？") { Store.report(Api.delete(s"/api/automations/${r.id}"))(_ => load(s)) }

def toggle(s: State, r: RuleDto): Unit =
  Store.report(Api.putUnit(s"/api/automations/${r.id}", RuleReq(r.name, !r.enabled, r.trigger, r.actions, r.runAs.map(_.id)).asJson))(_ => load(s))

/** テンプレート（トリガーの種類・条件・アクション）. */
val templates: List[(String, String, String, String, String)] = List(
  ("完了したらコメントする", "status_changed", "", """[{"type": "comment", "text": "{{actor.name}} さんが完了にしました（{{today}}）"}]""", ""),
  ("担当者がいなければ報告者を担当にする", "task_created", """{"field": "task.assignee", "op": "empty"}""", """[{"type": "assign", "user": "reporter"}]""", ""),
  ("バグにはサブタスク「再現手順の確認」を作る", "task_created", """{"field": "task.labels", "op": "contains", "value": "バグ"}""",
    """[{"type": "create_subtask", "title": "再現手順の確認: {{task.title}}", "assignee": "{{task.assignee.id}}"}]""", ""),
  ("毎朝、期限切れのタスクを担当者に通知する", "scheduled", "",
    """[{"type": "notify", "users": "assignee", "message": "期日（{{task.dueDate}}）を過ぎています"}]""", "is:overdue"),
  ("外部 API を呼んで結果で分岐する", "manual", "", """[
  {"type": "web_request", "method": "POST", "url": "https://example.com/api/check",
   "headers": {"Content-Type": "application/json"}, "body": {"key": "{{task.key}}", "title": "{{task.title}}"}, "saveAs": "res"},
  {"type": "if", "condition": {"field": "res.status", "op": "eq", "value": 200},
   "then": [{"type": "comment", "text": "チェック結果: {{res.body.result}}"}, {"type": "transition", "status": "完了"}],
   "else": [{"type": "comment", "text": "チェックに失敗しました（HTTP {{res.status}}）"}]}
]""", ""),
  ("受け取った Webhook からタスクを作る", "incoming_webhook", "",
    """[{"type": "create_task", "title": "{{webhookData.title | default:外部からの依頼}}", "description": "{{webhookData.body}}"}]""", ""),
  ("サブタスクがすべて完了したら親を完了にする", "status_changed", "",
    """[{"type": "for_each", "target": "parent", "actions": [
  {"type": "if", "condition": {"field": "task.subtasks.status", "op": "not_in", "value": "作業前,進行中,待機中"},
   "then": [{"type": "transition", "status": "完了"}]}]}]""", "")
)

def applyTemplateName(s: State, name: String): Unit = {
  templates.find(_._1 == name).foreach { (n, tt, c, a, q) =>
    if (s.name.v.isEmpty) s.name := n
    s.ttype := tt; s.condition := c; s.actions := a; s.query := q
    if (tt == "status_changed" && n.startsWith("完了")) s.to := "完了"
    if (tt == "scheduled") s.time := "09:00"
  }
}

def isType(s: State, t: String) = s.ttype.mapC(_ == t)
def members = Store.meta.mapC(_.map(_.members.map(_.user)).getOrElse(Nil))
def runClass(r: RunDto): String = r.status match { case "success" => "badge cat-done"; case "error" => "badge cat-waiting"; case _ => "badge" }
def logText(r: RunDto): String = r.log.asArray.toList.flatten.map(l => l.hcursor.get[String]("message").getOrElse("")).mkString(" / ")
</script>

<template>
  <section>
    <div class="row"><h2>自動化</h2><span class="grow"></span><button v-if="Store.isProjectAdmin" class="primary small" @click="edit(s, None)">＋ ルールを作成</button></div>
    <p class="muted small">イベントや時刻をきっかけに、条件に合えばアクション（項目の変更・コメント・通知・メール・Web API の呼び出し・条件分岐など）を実行します。
      アクションは「実行するユーザー」の権限で行われます（未指定ならルールの作成者）。</p>
    <div class="table-wrap">
      <table class="list">
        <thead><tr><th>名前</th><th>トリガー</th><th>有効</th><th>前回の定期実行</th><th></th></tr></thead>
        <tbody>
          <tr v-for="r in s.rules" :key="r.id">
            <td>{{ r.name }}<div v-if="r.incomingUrl.isDefined" class="small"><a href="#" @click.prevent='Dom.copy(r.incomingUrl.getOrElse(""), "URL をコピーしました")'>受信 URL をコピー</a></div></td>
            <td class="small">{{ triggerLabel(r.trigger.hcursor.get[String]("type").getOrElse("")) }}</td>
            <td><input type="checkbox" :checked="r.enabled" :disabled="Store.isProjectAdmin.mapC(!_)" @change="toggle(s, r)"></td>
            <td class="small muted">{{ r.lastRunAt.map(Fmt.ago).getOrElse("") }}</td>
            <td class="nowrap"><button v-if="Store.isProjectAdmin" class="ghost small" @click="edit(s, Some(r))">編集</button><button v-if="Store.isProjectAdmin" class="ghost small" @click="remove(s, r)">削除</button></td>
          </tr>
          <tr v-if="s.rules.mapC(_.isEmpty)"><td colspan="5" class="muted">ルールはありません</td></tr>
        </tbody>
      </table>
    </div>
    <h3>実行結果 <button class="ghost small" @click="load(s)">更新</button></h3>
    <div class="table-wrap">
      <table class="list">
        <tbody>
          <tr v-for="r in s.runs" :key="r.id">
            <td class="small nowrap">{{ Fmt.dateTime(r.startedAt) }}</td><td class="small">{{ r.ruleName }}</td>
            <td class="key">{{ r.taskKey.getOrElse("") }}</td><td><span :class="runClass(r)">{{ r.status }}</span></td><td class="small muted">{{ logText(r) }}</td>
          </tr>
          <tr v-if="s.runs.mapC(_.isEmpty)"><td class="muted">まだ実行されていません</td></tr>
        </tbody>
      </table>
    </div>

    <Modal v-if="s.editing.mapC(_.isDefined)" :onClose="_ => s.editing := None" :wide="true">
      <form @submit.prevent="save(s)">
          <h2>自動化のルール</h2>
          <div class="row">
            <label class="field grow"><span>名前</span><input type="text" v-model="s.name"></label>
            <div class="field"><span>テンプレート</span>
              <Combo :options="Opt.names(templates.map(_._1))" :keep="false" placeholder="（選ぶと入力されます）" :onChange="v => applyTemplateName(s, v)" />
            </div>
          </div>
          <label class="small"><input type="checkbox" v-model="s.enabled"> 有効</label>
          <div class="field"><span>トリガー</span><Combo :bind="Some(s.ttype)" :options="Opt.of(triggerTypes)" /></div>
          <div v-if='isType(s, "status_changed")' class="row">
            <label class="field grow"><span>変更前（ステータス名かカテゴリ。カンマ区切り。空ならどれでも）</span><input type="text" v-model="s.from"></label>
            <label class="field grow"><span>変更後</span><input type="text" v-model="s.to" placeholder="完了"></label>
          </div>
          <label v-if='isType(s, "task_updated")' class="field"><span>対象のフィールド（statusId, assigneeId, dueDate, labelIds, fields など。空ならどれでも）</span><input type="text" v-model="s.fields"></label>
          <div v-if='isType(s, "scheduled")' class="row">
            <label class="field"><span>毎日の時刻</span><input type="time" v-model="s.time"></label>
            <label class="field"><span>または間隔（分、5以上）</span><input type="number" v-model="s.interval"></label>
            <label class="field grow"><span>対象のタスク（検索条件。空ならタスクなしで1回）</span><input type="text" v-model="s.query" placeholder="is:overdue"></label>
          </div>
          <label v-if='isType(s, "incoming_webhook")' class="field"><span>対象のタスク（検索条件。本文に taskKey があればそのタスク。保存後に受信 URL が発行されます）</span><input type="text" v-model="s.query"></label>
          <label class="field"><span>条件（JSON、任意） <a href="#" @click.prevent="s.help := !s.help.v">書き方</a></span><textarea class="code small-code" v-model="s.condition" placeholder='{"field": "task.labels", "op": "contains", "value": "バグ"}'></textarea></label>
          <div v-if="s.help" class="panel small help">
            <p><b>条件</b>: <code>{"field": "パス", "op": "比較", "value": "値"}</code>、<code>{"all": [...]}</code>、<code>{"any": [...]}</code>、<code>{"not": {...}}</code>。
              比較は eq / neq / contains / not_contains / in / not_in / empty / not_empty / gt / gte / lt / lte / matches（正規表現）/ changed。</p>
            <p><b>スマート値</b>: <code>&#123;&#123;task.key&#125;&#125;</code> <code>&#123;&#123;task.title&#125;&#125;</code> <code>&#123;&#123;task.status.name&#125;&#125;</code>
              <code>&#123;&#123;task.assignee.name&#125;&#125;</code> <code>&#123;&#123;task.labels&#125;&#125;</code> <code>&#123;&#123;task.fields.名前&#125;&#125;</code>
              <code>&#123;&#123;task.url&#125;&#125;</code> <code>&#123;&#123;actor.name&#125;&#125;</code> <code>&#123;&#123;today+7&#125;&#125;</code>
              <code>&#123;&#123;webhookData.xxx&#125;&#125;</code> <code>&#123;&#123;res.body.xxx&#125;&#125;</code>（Web リクエストの結果）。
              <code>&#123;&#123;値 | default:なし&#125;&#125;</code> のように既定値も書けます。</p>
            <p><b>アクション</b>: set_field（field, value）/ transition（status）/ assign（user: reporter・actor・名前・メール・空で解除）/ add_label・remove_label（label）/
              add_watcher（user）/ comment（text、Markdown）/ create_subtask（title, assignee, description）/ create_task（title, projectKey, assignee, status）/
              set_sprint（sprint: active・backlog・名前）/ send_email（to, subject, body）/ notify（users: assignee,reporter,watchers / message）/
              web_request（method, url, headers, body, saveAs）/ set_var（name, value）/ if（condition, then, else）/ for_each（target: subtasks・parent・linked, actions）/
              log（message）</p>
          </div>
          <label class="field"><span>アクション（JSON の配列）</span><textarea class="code" v-model="s.actions"></textarea></label>
          <div class="field"><span>実行するユーザー</span>
            <Combo :bind="Some(s.runAs)" :live="Some(members.mapC(Opt.users))" clearLabel="ルールの作成者" /></div>
          <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
          <div class="actions"><button type="button" @click="s.editing := None">キャンセル</button><button type="submit" class="primary">保存</button></div>
        </form>
    </Modal>
  </section>
</template>

<style scoped>
.small-code { min-height: 4rem; }
.help code { font-size: .78rem; }
</style>
