<script lang="scala">
import org.scalajs.dom
import skea.web.parts.*

// 横断検索（すべてのプロジェクト）
def initial(): String = Option(new dom.URL(dom.window.location.href).searchParams.get("q")).getOrElse("")

def mounted(): Unit = pageTitle("検索")
</script>

<template>
  <div class="page">
    <h1>タスクを検索</h1>
    <TaskSearch :initial="initial()" />
  </div>
</template>
