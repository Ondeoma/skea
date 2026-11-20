<script lang="scala">
import skea.web.*
import skea.shared.*

// バックログのタスクの行（ID・タイトル・ラベル・ステータス・担当者）
case class Props(task: TaskDto)

def labels(props: Props): List[LabelDto] = Store.meta.v.toList.flatMap(_.labels).filter(l => props.task.labelIds.contains(l.id))
def badge(props: Props): String = "badge cat-" + Fmt.statusCat(Store.meta.v, props.task.statusId)
def status(props: Props): String = Fmt.statusName(Store.meta.v, props.task.statusId)
</script>

<template>
  <div class="trow">
    <span class="ic" :title="TaskType.label(props.task.taskType)">{{ TaskType.icon(props.task.taskType) }}</span>
    <span class="key">{{ props.task.key }}</span>
    <span class="grow ttl">{{ props.task.title }}</span>
    <span v-for="l in labels(props)" class="label-chip hide-sp" :style='"background:" + l.color'>{{ l.name }}</span>
    <span class="ic" :title='"優先度: " + Priority.label(props.task.priority)'>{{ Priority.icon(props.task.priority) }}</span>
    <span :class="badge(props)">{{ status(props) }}</span>
    <Avatar :user="props.task.assignee.map(person)" :size="22" />
  </div>
</template>

<style scoped>
.trow { display: flex; align-items: center; gap: .6rem; background: var(--card); border: 1px solid var(--line); border-radius: 6px; padding: .4rem .6rem;
  cursor: pointer; font-size: .88rem; }
.trow:hover { border-color: var(--accent); }
.ic { font-size: .8rem; width: 1.2em; text-align: center; flex: none; }
.ttl { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
</style>
