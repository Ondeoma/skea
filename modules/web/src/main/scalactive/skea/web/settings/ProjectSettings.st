<script lang="scala">
import skea.web.*
import io.github.ondeoma.scalactive.components.routes.RouterComponent

// プロジェクトの設定（タブごとのルート）
def base: String = Store.meta.v.map(m => s"/p/${m.project.key}/settings").getOrElse("/")

val routes: PathPatternRoutes = List(
  ("general", _ => General()),
  ("members", _ => Members()),
  ("helpdesk", _ => Helpdesk()),
  ("statuses", _ => Statuses()),
  ("labels", _ => Labels()),
  ("fields", _ => Fields()),
  ("templates", _ => Templates()),
  ("import", _ => Import()),
  ("automations", _ => Automations()),
  ("webhooks", _ => Webhooks()),
  ("repos", _ => Repos()),
)

/** 左のメニュー（見出しごとにまとめる）. */
val groups: List[(String, List[(String, String)])] = List(
  "プロジェクト" -> List("general" -> "一般", "members" -> "メンバーと権限", "helpdesk" -> "ヘルプデスク"),
  "課題の項目" -> List("statuses" -> "ステータス", "labels" -> "ラベル", "fields" -> "カスタムフィールド", "templates" -> "テンプレート・繰り返し"),
  "自動化と連携" -> List("import" -> "取り込み（CSV・Jira）", "automations" -> "自動化", "webhooks" -> "Webhook", "repos" -> "リポジトリ連携"),
)

def mounted(): Unit = pageTitle("設定")
</script>

<template>
  <div class="page">
    <div class="settings">
      <nav class="side-nav">
        <div v-for="g in groups" class="side-group">
          <div class="side-title">{{ g._1 }}</div>
          <RouterLink v-for="i in g._2" :to='base + "/" + i._1'>{{ i._2 }}</RouterLink>
        </div>
      </nav>
      <div class="content">
        <p v-if="Store.isProjectAdmin.mapC(!_)" class="muted small">設定を変えるにはプロジェクトの管理権限が必要です（閲覧のみ）。</p>
        <RouterComponent :routes="routes" :prefix="base" />
      </div>
    </div>
  </div>
</template>

<style scoped>
.settings { display: grid; grid-template-columns: 220px 1fr; gap: 2rem; align-items: start; }
.content { min-width: 0; }
@media (max-width: 720px) {
  .settings { grid-template-columns: 1fr; gap: 1rem; }
}
</style>
