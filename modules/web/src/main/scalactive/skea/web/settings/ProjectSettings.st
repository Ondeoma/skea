<script lang="scala">
import skea.web.*
import io.github.ondeoma.scalactive.components.routes.RouterComponent

// プロジェクトの設定（タブごとのルート）
def base: String = Store.meta.v.map(m => s"/p/${m.project.key}/settings").getOrElse("/")

val routes: PathPatternRoutes = List(
  ("general", _ => General()),
  ("members", _ => Members()),
  ("statuses", _ => Statuses()),
  ("labels", _ => Labels()),
  ("fields", _ => Fields()),
  ("templates", _ => Templates()),
  ("import", _ => Import()),
  ("automations", _ => Automations()),
  ("webhooks", _ => Webhooks()),
  ("repos", _ => Repos()),
)


def mounted(): Unit = pageTitle("設定")
</script>

<template>
  <div class="page">
    <div class="settings">
      <nav class="side-nav">
        <RouterLink :to='base + "/general"'>一般</RouterLink>
        <RouterLink :to='base + "/members"'>メンバーと権限</RouterLink>
        <RouterLink :to='base + "/statuses"'>ステータス</RouterLink>
        <RouterLink :to='base + "/labels"'>ラベル</RouterLink>
        <RouterLink :to='base + "/fields"'>カスタムフィールド</RouterLink>
        <RouterLink :to='base + "/templates"'>テンプレート・繰り返し</RouterLink>
        <RouterLink :to='base + "/import"'>取り込み（CSV・Jira）</RouterLink>
        <RouterLink :to='base + "/automations"'>自動化</RouterLink>
        <RouterLink :to='base + "/webhooks"'>Webhook</RouterLink>
        <RouterLink :to='base + "/repos"'>リポジトリ連携</RouterLink>
      </nav>
      <div class="content">
        <p v-if="Store.isProjectAdmin.mapC(!_)" class="muted small">設定を変えるにはプロジェクトの管理権限が必要です（閲覧のみ）。</p>
        <RouterComponent :routes="routes" :prefix="base" />
      </div>
    </div>
  </div>
</template>

<style scoped>
.settings { display: grid; grid-template-columns: 200px 1fr; gap: 1.5rem; }
.side-nav { display: flex; flex-direction: column; gap: 2px; }
.side-nav a { padding: .45rem .7rem; border-radius: 7px; color: var(--fg); font-size: .9rem; }
.side-nav a:hover { background: var(--hover); text-decoration: none; }
.side-nav a.router-link-active { background: var(--accent-weak); color: var(--accent); }
.content { min-width: 0; }
@media (max-width: 720px) {
  .settings { grid-template-columns: 1fr; }
  .side-nav { flex-direction: row; overflow-x: auto; }
  .side-nav a { white-space: nowrap; }
}
</style>
