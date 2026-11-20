<script lang="scala">
import skea.web.*
import skea.web.parts.*
import io.circe.syntax.*
import org.scalajs.dom
import skea.shared.*

// CSV・Jira（課題を CSV で書き出したもの）からの取り込み. 列の対応は見出しから推し、画面で直せる.
case class State(fileName: RV[String] = RV(""),
                 headers: RV[List[String]] = RV(Nil),
                 rows: RV[List[List[String]]] = RV(Nil),
                 mapping: RV[Map[String, String]] = RV(Map.empty),
                 result: RV[Option[(Boolean, ImportResultDto)]] = RV(None),
                 act: ActionState)

def initState() = State(act = useAction())

/** 取り込む項目と、その見出しの候補（大文字小文字は区別しない）. */
val targets: List[(String, String, List[String])] = List(
  ("title", "タイトル（必須）", List("summary", "タイトル", "title", "概要", "件名")),
  ("ref", "元のID（親子をつなぐ）", List("issue key", "key", "id", "キー", "課題キー")),
  ("parentRef", "親の元のID", List("parent", "parent key", "親", "親課題", "親タスク")),
  ("description", "説明", List("description", "説明", "本文")),
  ("status", "ステータス", List("status", "ステータス", "状態")),
  ("taskType", "種類", List("issue type", "type", "種類", "トラッカー")),
  ("priority", "優先度", List("priority", "優先度")),
  ("assignee", "担当者（名前かメール）", List("assignee", "担当者", "担当")),
  ("reporter", "報告者（名前かメール）", List("reporter", "報告者", "起票者")),
  ("labels", "ラベル", List("labels", "label", "ラベル")),
  ("startDate", "開始日", List("start date", "開始日", "start")),
  ("dueDate", "期日", List("due date", "期日", "期限", "期限日", "due")),
  ("sprint", "スプリント", List("sprint", "スプリント"))
)

/** 見出しから列の対応を推す. Jira で Issue id と Parent id があれば、親子はそれでつなぐ. */
def guess(headers: List[String]): Map[String, String] = {
  val low = headers.map(_.trim.toLowerCase)
  def idx(names: List[String]): Option[Int] = names.iterator.map(n => low.indexOf(n)).find(_ >= 0)
  val base = targets.flatMap((k, _, names) => idx(names).map(i => k -> i.toString)).toMap
  (idx(List("issue id")), idx(List("parent id"))) match {
    case (Some(i), Some(p)) => base ++ Map("ref" -> i.toString, "parentRef" -> p.toString)
    case _                  => base
  }
}

def isJira(s: State) = s.headers.mapC(h => h.exists(_.trim.equalsIgnoreCase("Issue key")))

def pick(s: State, e: dom.Event): Unit = {
  Dom.takeFiles(e).headOption.foreach { file =>
    val reader = new dom.FileReader()
    reader.onload = (_: dom.Event) => {
      val all = Csv.parse(reader.result.asInstanceOf[String])
      s.fileName := file.name
      s.headers := all.headOption.getOrElse(Nil)
      s.rows := all.drop(1)
      s.mapping := guess(all.headOption.getOrElse(Nil))
      s.result := None
    }
    reader.readAsText(file, "UTF-8")
  }
}

def setMap(s: State, key: String, v: String): Unit = s.mapping := (if (v.isEmpty) s.mapping.v - key else s.mapping.v.updated(key, v))

/** 取り込む行（ラベルは同じ見出しの列をすべてまとめ、, ; で区切る）. */
def build(s: State): List[ImportRow] = {
  val m = s.mapping.v
  val hs = s.headers.v
  val labelCols = m.get("labels").flatMap(_.toIntOption).map(i => hs.indices.filter(j => hs(j).trim.equalsIgnoreCase(hs(i).trim)).toList).getOrElse(Nil)
  s.rows.v.map { r =>
    def get(k: String): Option[String] = m.get(k).flatMap(_.toIntOption).flatMap(r.lift).map(_.trim).filter(_.nonEmpty)
    ImportRow(title = get("title").getOrElse(""), ref = get("ref"), description = get("description"), status = get("status"),
      assignee = get("assignee"), reporter = get("reporter"),
      labels = labelCols.flatMap(r.lift).flatMap(_.split("[,;、]").map(_.trim)).filter(_.nonEmpty).distinct,
      startDate = get("startDate"), dueDate = get("dueDate"), parentRef = get("parentRef"), sprint = get("sprint"),
      priority = get("priority"), taskType = get("taskType"))
  }.filter(r => r.title.nonEmpty || r.ref.nonEmpty)
}

def run(s: State, dryRun: Boolean): Unit = {
  val rows = build(s)
  if (rows.isEmpty) Toasts.show("取り込む行がありません（タイトルの列を選んでください）", error = true)
  else {
    Toasts.report(s.act.run(Api.post[ImportResultDto](s"/api/projects/${Store.pid}/import", ImportReq(rows, dryRun).asJson))) { r =>
      s.result := Some((dryRun, r))
      if (!dryRun) { Toasts.show(s"${r.created} 件のタスクを取り込みました"); Store.reloadMeta(); Store.changed() }
    }
  }
}

def preview(s: State) = computed { build(s).take(8) }
def count(s: State) = computed { build(s).size }
def headerOpts(s: State) = s.headers.mapC(_.zipWithIndex.map((h, i) => Opt(i.toString, h)))
def loaded(s: State) = s.headers.mapC(_.nonEmpty)
def resultTitle(s: State) = s.result.mapC(_.fold("") { (dry, r) => if (dry) s"確認: ${r.created} 件を取り込めます" else s"${r.created} 件を取り込みました" })
def warnings(s: State) = s.result.mapC(_.map(_._2.warnings).getOrElse(Nil))
def headerIdx(s: State) = s.headers.mapC(_.zipWithIndex)
def template(): Unit = {
  val head = List("タイトル", "ID", "親", "説明", "ステータス", "担当者", "報告者", "ラベル", "開始日", "期日", "スプリント").map(Csv.quote).mkString(",")
  val ex = List("ログイン画面を作る", "A-1", "", "画面の設計と実装", "進行中", "", "", "UI,画面", "2026-11-16", "2026-11-30", "").map(Csv.quote).mkString(",")
  val ex2 = List("パスワードの再設定", "A-2", "A-1", "", "作業前", "", "", "", "", "2026-12-05", "").map(Csv.quote).mkString(",")
  Csv.download("skea-import-sample.csv", List(head, ex, ex2))
}
</script>

<template>
  <section>
    <h2>取り込み（CSV・Jira）</h2>
    <p class="muted small">CSV のタスクを、このプロジェクトに取り込みます。Jira は「課題の検索 → エクスポート → CSV（すべてのフィールド）」で書き出したファイルをそのまま使えます。
      ステータス・担当者・ラベル・スプリントは名前で探し、見つからないステータスは名前から推した区分（完了・進行中など）にします。ラベルはなければ作ります。
      担当の通知は送りません。</p>
    <div class="row">
      <label class="btn primary small">CSV ファイルを選ぶ<input type="file" accept=".csv,text/csv" class="hidden" @change="pick(s, event)"></label>
      <span class="small">{{ s.fileName }}</span>
      <span v-if="isJira(s)" class="badge cat-in_progress">Jira の書き出し</span>
      <span class="grow"></span>
      <a href="#" class="small" @click.prevent="template()">見本の CSV</a>
    </div>
    <template v-if="loaded(s)">
      <h3>列の対応（{{ count(s) }} 行）</h3>
      <div class="maps">
        <div v-for="t in targets" class="field"><span>{{ t._2 }}</span>
          <Combo :live="Some(headerOpts(s))" :shows="Some(s.mapping.mapC(_.getOrElse(t._1, &quot;&quot;)))" clearLabel="（使わない）" :small="true"
            :onChange="v => setMap(s, t._1, v)" />
        </div>
      </div>
      <h3>先頭の行</h3>
      <div class="table-wrap">
        <table class="list"><thead><tr><th>ID</th><th>タイトル</th><th>ステータス</th><th>担当者</th><th>ラベル</th><th>期日</th><th>親</th></tr></thead>
          <tbody><tr v-for="r in preview(s)">
            <td class="key">{{ r.ref.getOrElse("") }}</td><td>{{ r.title }}</td><td>{{ r.status.getOrElse("") }}</td><td>{{ r.assignee.getOrElse("") }}</td>
            <td class="small">{{ r.labels.mkString(", ") }}</td><td class="small">{{ r.dueDate.getOrElse("") }}</td><td class="key">{{ r.parentRef.getOrElse("") }}</td>
          </tr></tbody>
        </table>
      </div>
      <div class="row acts">
        <button :disabled="s.act.busy" @click="run(s, true)">確認する（まだ作らない）</button>
        <button class="primary" :disabled="s.act.busy" @click="run(s, false)">取り込む</button>
        <span v-if="s.act.busy" class="muted small">処理中…</span>
      </div>
      <div v-if="s.result.mapC(_.isDefined)" class="panel">
        <b>{{ resultTitle(s) }}</b>
        <ul v-if="warnings(s).mapC(_.nonEmpty)" class="small warn"><li v-for="w in warnings(s)">{{ w }}</li></ul>
      </div>
    </template>
  </section>
</template>

<style scoped>
.maps { display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); gap: 0 .8rem; }
.acts { margin: .8rem 0; }
.warn { max-height: 240px; overflow-y: auto; margin: .4rem 0 0; padding-left: 1.2rem; color: var(--warn); }
</style>
