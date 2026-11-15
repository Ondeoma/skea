<script lang="scala">
import skea.web.*
// タスクの作成ダイアログ（Store.createDialog に作成先を入れると開く）
def isOpen = Store.createDialog.mapC(_.isDefined)
def target = Store.createDialog.mapC(_.toList)
</script>

<template>
  <Modal v-if="isOpen" :onClose="_ => Store.createDialog := None">
    <div v-for="t in target">
        <TaskForm :projectId="t._1" :parentId="t._2" />
      </div>
  </Modal>
</template>
