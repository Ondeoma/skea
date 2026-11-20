<script lang="scala">
import skea.web.*
import skea.shared.*

// カンバン・バックログのタスクのカード
case class Props(task: TaskDto, meta: Option[ProjectMetaDto], compact: Boolean = false)

def labels(props: Props): List[LabelDto] = props.meta.toList.flatMap(_.labels).filter(l => props.task.labelIds.contains(l.id))
def done(props: Props): Boolean = Fmt.statusCat(props.meta, props.task.statusId) == StatusCategory.Done
def dueClass(props: Props): String = if (Fmt.isOverdue(props.task.dueDate, done(props))) "due overdue" else "due"
def subtasks(props: Props): String = s"☑ ${props.task.doneSubtaskCount}/${props.task.subtaskCount}"
def parentKey(props: Props): String = props.task.parentId.flatMap(_ => Option("↳")).getOrElse("")
</script>

<template>
  <div :class='(if (props.compact) "tcard compact" else "tcard") + " prio-" + props.task.priority' :title='"優先度: " + Priority.label(props.task.priority)'>
    <div class="t-title"><span class="ttype" :title="TaskType.label(props.task.taskType)">{{ TaskType.icon(props.task.taskType) }}</span>{{ props.task.title }}</div>
    <div v-if="labels(props).nonEmpty" class="t-labels">
      <span v-for="l in labels(props)" :key="l.id" class="label-chip" :style='"background:" + l.color'>{{ l.name }}</span>
    </div>
    <div class="t-meta">
      <span class="key">{{ parentKey(props) }}{{ props.task.key }}</span>
      <span v-if="props.task.priority != Priority.Medium" class="prio" :title='"優先度: " + Priority.label(props.task.priority)'>{{ Priority.icon(props.task.priority) }}</span>
      <span v-if="props.task.dueDate.isDefined" :class="dueClass(props)">📅 {{ props.task.dueDate.map(Fmt.day).getOrElse("") }}</span>
      <span v-if="props.task.subtaskCount > 0" class="muted">{{ subtasks(props) }}</span>
      <span v-if="props.task.commentCount > 0" class="muted">💬 {{ props.task.commentCount }}</span>
      <span class="grow"></span>
      <Avatar :user="props.task.assignee.map(person)" :size="22" />
    </div>
  </div>
</template>

<style scoped>
.tcard { background: var(--card); border: 1px solid var(--line); border-radius: 8px; padding: .55rem .65rem; cursor: pointer; box-shadow: 0 1px 2px rgba(0,0,0,.05); }
.tcard:hover { border-color: var(--accent); }
/* 優先度を左端の色で表す（中は色なし） */
.tcard { border-left-width: 4px; }
.tcard.prio-highest { border-left-color: #dc2626; }
.tcard.prio-high { border-left-color: #f97316; }
.tcard.prio-medium { border-left-color: var(--line); }
.tcard.prio-low { border-left-color: #60a5fa; }
.tcard.prio-lowest { border-left-color: #94a3b8; }
.t-title { font-size: .9rem; line-height: 1.4; margin-bottom: .35rem; word-break: break-word; }
.t-labels { display: flex; flex-wrap: wrap; gap: .25rem; margin-bottom: .35rem; }
.t-meta { display: flex; align-items: center; gap: .5rem; font-size: .75rem; flex-wrap: wrap; }
.due { color: var(--muted); }
.ttype { margin-right: .3rem; font-size: .8rem; }
.prio { font-size: .8rem; }
.compact .t-title { margin-bottom: .2rem; }
</style>
