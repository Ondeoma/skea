<script lang="scala">
import skea.web.*

// ヘルプデスク: 参加している窓口の一覧（1つだけならその窓口を開く）
def mounted(): Unit = {
  pageTitle("問い合わせ")
  Store.helpProjects.v match {
    case List(p) => Router.go(s"/help/${p.key}")
    case _       => ()
  }
}

def none = Store.helpProjects.mapC(_.isEmpty)
</script>

<template>
  <div class="page narrow">
    <h1>問い合わせ窓口</h1>
    <div class="portals">
      <div v-for="p in Store.helpProjects" :key="p.projectId" class="pcard" @click='Router.go("/help/" + p.key)'>
        <div class="pname">{{ p.name }}</div>
        <div class="muted small">{{ p.description }}</div>
      </div>
    </div>
    <p v-if="none" class="muted">参加している窓口はありません。案内された窓口の URL を開いてください。</p>
  </div>
</template>

<style scoped>
.portals { display: grid; gap: .7rem; }
.pcard { display: block; border: 1px solid var(--line); border-radius: 10px; padding: .9rem 1rem; color: var(--fg); background: var(--card); cursor: pointer; }
.pcard:hover { border-color: var(--accent); text-decoration: none; }
.pname { font-weight: 600; margin-bottom: .2rem; }
</style>
