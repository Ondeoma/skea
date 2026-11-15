<script lang="scala">
import skea.web.*
import skea.web.parts.*

// プロジェクトのタスク一覧（検索と同じ画面をプロジェクトに絞って使う）
def key: Option[String] = Store.meta.v.map(_.project.key)

def mounted(): Unit = pageTitle("一覧")
</script>

<template>
  <div class="page wide">
    <TaskSearch :projectKey="key" initial="is:open order:rank" />
  </div>
</template>
