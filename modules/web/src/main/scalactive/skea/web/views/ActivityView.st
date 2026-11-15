<script lang="scala">
import skea.web.*
import skea.shared.*
import skea.web.parts.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// プロジェクトの最近の活動
case class State(list: RV[List[EventDto]] = RV(Nil))

def initState() = State()

def load(s: State): Unit = Store.meta.v.foreach(m => Api.activity(m.project.id).foreach(s.list := _))

def mounted(s: State): Unit = {
  pageTitle("活動")
  load(s)
  watch(Store.tick, false)((_, _) => load(s))
}

</script>

<template>
  <div class="page narrow">
    <div v-for="e in s.list" :key="e.id" class="ev">
      <Avatar :user="e.actor" :size="26" />
      <div class="grow">
        <b>{{ Store.actorName(e) }}</b> さんが
        <RouterLink v-if="e.taskKey.isDefined" :to='"/tasks/" + e.taskKey.getOrElse("")'>{{ EventText.describe(e.kind, e.detail, e.taskKey) }}</RouterLink>
        <template v-else>{{ EventText.describe(e.kind, e.detail, e.taskKey) }}</template>
        <div class="muted small">{{ Fmt.dateTime(e.at) }} {{ if (e.via == "web") "" else "（" + e.via + "）" }}</div>
      </div>
    </div>
    <p v-if="s.list.mapC(_.isEmpty)" class="empty">まだ活動はありません</p>
  </div>
</template>

<style scoped>
.ev { display: flex; gap: .7rem; padding: .55rem 0; border-bottom: 1px solid var(--line); font-size: .9rem; }
</style>
