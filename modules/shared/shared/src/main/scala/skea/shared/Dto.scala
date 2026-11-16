package skea.shared

import io.circe.*
import io.circe.derivation.{Configuration, ConfiguredDecoder}

import java.util.UUID

// バックエンド(JVM)と画面(Scala.js)の両方で使う API の型。
// 日時は Timestamp（JVM では Instant）、日付は "yyyy-MM-dd" の文字列（Scala.js で java.time を使わないため）。

final case class ErrorDto(error: String) derives Encoder.AsObject, Decoder

/** 権限とカテゴリなど、文字列で表す値. */
object Roles {
  val View = "view"
  val Edit = "edit"
  val Admin = "admin"
  val all: List[String] = List(View, Edit, Admin)
  def rank(r: String): Int = r match {
    case View  => 1
    case Edit  => 2
    case Admin => 3
    case _     => 0
  }
  def label(r: String): String = r match {
    case View  => "閲覧"
    case Edit  => "編集"
    case Admin => "管理"
    case _     => r
  }
}

object StatusCategory {
  val Todo = "todo"
  val InProgress = "in_progress"
  val Waiting = "waiting"
  val Done = "done"
  val all: List[String] = List(Todo, InProgress, Waiting, Done)
  def label(c: String): String = c match {
    case Todo       => "作業前"
    case InProgress => "進行中"
    case Waiting    => "待機中"
    case Done       => "完了"
    case _          => c
  }
}

/** タスクの優先度（rank が大きいほど高い）. */
object Priority {
  val Highest = "highest"
  val High = "high"
  val Medium = "medium"
  val Low = "low"
  val Lowest = "lowest"
  val all: List[String] = List(Highest, High, Medium, Low, Lowest)
  def label(p: String): String = p match {
    case Highest => "最高"
    case High    => "高"
    case Medium  => "中"
    case Low     => "低"
    case Lowest  => "最低"
    case _       => p
  }
  def icon(p: String): String = p match {
    case Highest => "⏫"
    case High    => "🔼"
    case Low     => "🔽"
    case Lowest  => "⏬"
    case _       => "⏺"
  }
  def rank(p: String): Int = 4 - all.indexOf(p)
  /** 名前・別名（Jira の Highest / Major など）から. */
  def parse(s: String): Option[String] = s.trim.toLowerCase match {
    case "highest" | "最高" | "blocker" | "critical" | "緊急" => Some(Highest)
    case "high" | "高" | "major" | "重要"                      => Some(High)
    case "medium" | "中" | "normal" | "普通"                   => Some(Medium)
    case "low" | "低" | "minor"                               => Some(Low)
    case "lowest" | "最低" | "trivial"                        => Some(Lowest)
    case _                                                    => None
  }
}

/** タスクの種類. */
object TaskType {
  val Task = "task"
  val Bug = "bug"
  val Story = "story"
  val Epic = "epic"
  val Improvement = "improvement"
  val all: List[String] = List(Task, Bug, Story, Epic, Improvement)
  def label(t: String): String = t match {
    case Task        => "タスク"
    case Bug         => "バグ"
    case Story       => "ストーリー"
    case Epic        => "エピック"
    case Improvement => "改善"
    case _           => t
  }
  def icon(t: String): String = t match {
    case Bug         => "🐞"
    case Story       => "📗"
    case Epic        => "⚡"
    case Improvement => "✨"
    case _           => "✅"
  }
  def parse(s: String): Option[String] = s.trim.toLowerCase match {
    case "task" | "タスク" | "sub-task" | "subtask" | "サブタスク" => Some(Task)
    case "bug" | "バグ" | "不具合" | "defect"                      => Some(Bug)
    case "story" | "ストーリー" | "user story"                     => Some(Story)
    case "epic" | "エピック"                                        => Some(Epic)
    case "improvement" | "改善" | "enhancement" | "new feature" | "feature" => Some(Improvement)
    case _                                                         => None
  }
}

object FieldKind {
  val Text = "text"
  val Textarea = "textarea"
  val Checkbox = "checkbox"
  val Radio = "radio"
  val Select = "select"
  val all: List[String] = List(Text, Textarea, Checkbox, Radio, Select)
  def label(k: String): String = k match {
    case Text     => "テキスト"
    case Textarea => "複数行テキスト"
    case Checkbox => "チェックボックス"
    case Radio    => "ラジオボタン"
    case Select   => "選択肢"
    case _        => k
  }
  def hasOptions(k: String): Boolean = k == Checkbox || k == Radio || k == Select
}

object LinkKind {
  val Relates = "relates"
  val Blocks = "blocks"
  val IsBlockedBy = "is_blocked_by"
  val Duplicates = "duplicates"
  val IsDuplicatedBy = "is_duplicated_by"
  val Clones = "clones"
  val Web = "web"
  val taskKinds: List[String] = List(Relates, Blocks, IsBlockedBy, Duplicates, IsDuplicatedBy, Clones)
  def label(k: String): String = k match {
    case Relates        => "関連"
    case Blocks         => "ブロックしている"
    case IsBlockedBy    => "ブロックされている"
    case Duplicates     => "重複している"
    case IsDuplicatedBy => "重複されている"
    case Clones         => "複製元"
    case Web            => "Webリンク"
    case _              => k
  }
  /** 相手側から見たリンクの種類. */
  def inverse(k: String): String = k match {
    case Blocks         => IsBlockedBy
    case IsBlockedBy    => Blocks
    case Duplicates     => IsDuplicatedBy
    case IsDuplicatedBy => Duplicates
    case other          => other
  }
}

object NotificationKind {
  val Mention = "mention"
  val Assigned = "assigned"
  val Comment = "comment"
  val Updated = "updated"
  val Status = "status"
  val Due = "due"
  val Automation = "automation"
  val all: List[String] = List(Mention, Assigned, Comment, Updated, Status, Due, Automation)
  def label(k: String): String = k match {
    case Mention    => "メンション"
    case Assigned   => "担当"
    case Comment    => "コメント"
    case Updated    => "更新"
    case Status     => "ステータス"
    case Due        => "期限"
    case Automation => "自動化"
    case _          => k
  }
}

object ApiScope {
  val Read = "read"
  val Write = "write"
  val Admin = "admin"
  val all: List[String] = List(Read, Write, Admin)
  def label(s: String): String = s match {
    case Read  => "参照"
    case Write => "作成・更新・削除"
    case Admin => "管理"
    case _     => s
  }
}

/** Webhook・自動化で使うイベント名. */
object EventKind {
  val TaskCreated = "task.created"
  val TaskUpdated = "task.updated"
  val TaskDeleted = "task.deleted"
  val StatusChanged = "task.status_changed"
  val CommentAdded = "comment.added"
  val CommentUpdated = "comment.updated"
  val CommentDeleted = "comment.deleted"
  val WorklogAdded = "worklog.added"
  val SprintCreated = "sprint.created"
  val SprintStarted = "sprint.started"
  val SprintCompleted = "sprint.completed"
  val CommitLinked = "commit.linked"
  val AttachmentAdded = "attachment.added"
  val all: List[String] = List(TaskCreated, TaskUpdated, TaskDeleted, StatusChanged, CommentAdded, CommentUpdated, CommentDeleted,
    WorklogAdded, SprintCreated, SprintStarted, SprintCompleted, CommitLinked, AttachmentAdded)
  def label(k: String): String = k match {
    case TaskCreated     => "タスクの作成"
    case TaskUpdated     => "タスクの更新"
    case TaskDeleted     => "タスクの削除"
    case StatusChanged   => "ステータスの変更"
    case CommentAdded    => "コメントの追加"
    case CommentUpdated  => "コメントの編集"
    case CommentDeleted  => "コメントの削除"
    case WorklogAdded    => "作業時間の記録"
    case SprintCreated   => "スプリントの作成"
    case SprintStarted   => "スプリントの開始"
    case SprintCompleted => "スプリントの完了"
    case CommitLinked    => "コミットのリンク"
    case AttachmentAdded => "添付ファイルの追加"
    case _               => k
  }
}

// ------------------------------------------------------------------ ユーザー

final case class UserBrief(id: UUID, name: String, avatarUrl: Option[String]) derives Encoder.AsObject, Decoder

final case class UserDto(id: UUID, name: String, email: Option[String], avatarUrl: Option[String], isAdmin: Boolean, disabled: Boolean,
    lastLoginAt: Option[Timestamp], groups: List[String]) derives Encoder.AsObject, Decoder

final case class MeDto(id: UUID, name: String, email: Option[String], avatarUrl: Option[String], isAdmin: Boolean, timezone: String,
    via: String, scopes: List[String]) derives Encoder.AsObject, Decoder

final case class UpdateMeReq(name: Option[String], timezone: Option[String]) derives Encoder.AsObject, Decoder

final case class UpdateUserReq(isAdmin: Option[Boolean], disabled: Option[Boolean]) derives Encoder.AsObject, Decoder

/** 通知の種類ごとに、メール・チャット（stui）にも送るか. */
final case class NotifyPrefDto(kind: String, email: Boolean, chat: Boolean) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ APIキー

final case class ApiKeyDto(id: UUID, name: String, prefix: String, scopes: List[String], projectIds: Option[List[UUID]],
    createdAt: Timestamp, expiresAt: Option[Timestamp], lastUsedAt: Option[Timestamp]) derives Encoder.AsObject, Decoder
final case class CreateApiKeyReq(name: String, scopes: List[String], projectIds: Option[List[UUID]], expiresInDays: Option[Int])
    derives Encoder.AsObject, Decoder
final case class CreatedApiKeyDto(key: String, info: ApiKeyDto) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ プロジェクト

final case class ProjectDto(id: UUID, key: String, name: String, description: String, lead: Option[UserBrief], archived: Boolean,
    role: String, createdAt: Timestamp) derives Encoder.AsObject, Decoder
final case class CreateProjectReq(key: String, name: String, description: Option[String]) derives Encoder.AsObject, Decoder
final case class UpdateProjectReq(name: Option[String], description: Option[String], leadId: Option[UUID], archived: Option[Boolean])
    derives Encoder.AsObject, Decoder
final case class ReorderReq(ids: List[UUID]) derives Encoder.AsObject, Decoder

final case class MemberDto(user: UserBrief, role: String) derives Encoder.AsObject, Decoder

/**
 * まとめてメンションする宛先. id は "g:<グループID>"（グループ）か "r:admin"・"r:edit"・"r:all"（プロジェクトの役割）.
 * count はそのプロジェクトで届く人数.
 */
final case class MentionGroupDto(id: String, name: String, count: Int) derives Encoder.AsObject, Decoder
final case class SetMemberReq(userId: UUID, role: String) derives Encoder.AsObject, Decoder
final case class GroupRoleDto(groupId: UUID, groupName: String, role: String) derives Encoder.AsObject, Decoder
final case class SetGroupRoleReq(groupName: String, role: String) derives Encoder.AsObject, Decoder

final case class StatusDto(id: UUID, name: String, category: String, color: String, position: Int) derives Encoder.AsObject, Decoder
final case class StatusReq(name: String, category: String, color: Option[String]) derives Encoder.AsObject, Decoder

final case class LabelDto(id: UUID, name: String, color: String) derives Encoder.AsObject, Decoder
final case class LabelReq(name: String, color: Option[String]) derives Encoder.AsObject, Decoder

final case class CustomFieldDto(id: UUID, name: String, kind: String, options: List[String], required: Boolean, position: Int)
    derives Encoder.AsObject, Decoder
final case class CustomFieldReq(name: String, kind: String, options: List[String], required: Boolean) derives Encoder.AsObject, Decoder

final case class SprintDto(id: UUID, projectId: UUID, name: String, goal: String, state: String, startDate: Option[String],
    endDate: Option[String], position: Int, completedAt: Option[Timestamp]) derives Encoder.AsObject, Decoder
final case class SprintReq(name: String, goal: Option[String], startDate: Option[String], endDate: Option[String])
    derives Encoder.AsObject, Decoder
/** スプリントの完了. 未完了のタスクの移動先（None ならバックログ）. */
final case class CompleteSprintReq(moveTo: Option[UUID]) derives Encoder.AsObject, Decoder

/** プロジェクトの画面で使う設定一式（1回で取得する）. */
final case class ProjectMetaDto(project: ProjectDto, statuses: List[StatusDto], labels: List[LabelDto], sprints: List[SprintDto],
    fields: List[CustomFieldDto], members: List[MemberDto], latestEventId: Long) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ タスク

/** 一覧・カンバン・ガントで使うタスクの要約. */
final case class TaskDto(
    id: UUID,
    key: String,
    projectId: UUID,
    parentId: Option[UUID],
    title: String,
    statusId: UUID,
    sprintId: Option[UUID],
    assignee: Option[UserBrief],
    reporter: Option[UserBrief],
    startDate: Option[String],
    dueDate: Option[String],
    spentMinutes: Int,
    rank: String,
    labelIds: List[UUID],
    subtaskCount: Int,
    doneSubtaskCount: Int,
    commentCount: Int,
    version: Int,
    createdAt: Timestamp,
    updatedAt: Timestamp,
    resolvedAt: Option[Timestamp],
    priority: String,
    taskType: String
) derives Encoder.AsObject, Decoder

/** 別のタスクへの参照（リンク・親など）. */
final case class TaskRef(id: UUID, key: String, title: String, statusName: String, statusCategory: String) derives Encoder.AsObject, Decoder

final case class TaskLinkDto(id: UUID, kind: String, target: Option[TaskRef], url: Option[String], title: String,
    createdAt: Timestamp) derives Encoder.AsObject, Decoder
final case class CreateLinkReq(kind: String, targetKey: Option[String], url: Option[String], title: Option[String])
    derives Encoder.AsObject, Decoder

final case class DependencyDto(predecessorId: UUID, successorId: UUID, kind: String) derives Encoder.AsObject, Decoder
final case class CreateDependencyReq(predecessorKey: String, kind: Option[String]) derives Encoder.AsObject, Decoder

final case class CommitDto(repoName: String, provider: String, sha: String, message: String, url: String, author: String,
    branch: Option[String], committedAt: Timestamp) derives Encoder.AsObject, Decoder

final case class AttachmentDto(id: UUID, fileName: String, size: Long, contentType: String, createdBy: Option[UserBrief],
    createdAt: Timestamp, url: String) derives Encoder.AsObject, Decoder

final case class TaskDetailDto(
    task: TaskDto,
    description: Option[Json],
    descriptionHtml: String,
    ancestors: List[TaskRef],
    subtasks: List[TaskDto],
    watchers: List[UserBrief],
    watching: Boolean,
    links: List[TaskLinkDto],
    predecessors: List[TaskRef],
    successors: List[TaskRef],
    fields: Map[String, Json],
    commits: List[CommitDto],
    attachments: List[AttachmentDto],
    role: String,
    latestEventId: Long
) derives Encoder.AsObject, Decoder

final case class CreateTaskReq(
    title: String,
    description: Option[Json] = None,
    statusId: Option[UUID] = None,
    sprintId: Option[UUID] = None,
    assigneeId: Option[UUID] = None,
    reporterId: Option[UUID] = None,
    parentId: Option[UUID] = None,
    startDate: Option[String] = None,
    dueDate: Option[String] = None,
    labelIds: List[UUID] = Nil,
    fields: Map[String, Json] = Map.empty,
    priority: Option[String] = None,
    taskType: Option[String] = None
) derives Encoder.AsObject

object CreateTaskReq {
  /** 省いたキーは既定値（labelIds・fields は空）にする. */
  private given Configuration = Configuration.default.withDefaults
  given Decoder[CreateTaskReq] = ConfiguredDecoder.derived[CreateTaskReq]
}

/**
 * タスクの更新は JSON のオブジェクトで、変えるキーだけを送る（null は「空にする」）.
 * キー: title, description, statusId, sprintId, assigneeId, reporterId, parentId, startDate, dueDate,
 * labelIds, fields（{フィールドID: 値}. 値が null なら削除）. version を付けると楽観ロックで競合を検出する.
 */
object TaskPatch {
  val keys: List[String] = List("title", "description", "statusId", "sprintId", "assigneeId", "reporterId", "parentId", "startDate",
    "dueDate", "labelIds", "fields", "priority", "taskType")

  /** フィールド名の表示（変更履歴・通知）. */
  def label(field: String): String = field match {
    case "title"           => "タイトル"
    case "description"     => "説明"
    case "statusId"        => "ステータス"
    case "sprintId"        => "スプリント"
    case "assigneeId"      => "担当者"
    case "reporterId"      => "報告者"
    case "parentId"        => "親タスク"
    case "startDate"       => "開始日"
    case "dueDate"         => "期日"
    case "labelIds"        => "ラベル"
    case "priority"        => "優先度"
    case "taskType"        => "種類"
    case "links"           => "リンク"
    case "dependencies"    => "依存関係"
    case "attachments"     => "添付ファイル"
    case f if f.startsWith("fields.") => "カスタムフィールド"
    case other             => other
  }
}

/** カンバン・バックログでの移動. afterId / beforeId の間に入れる. toBacklog ならスプリントから外す. */
final case class MoveTaskReq(statusId: Option[UUID], sprintId: Option[UUID], toBacklog: Option[Boolean], afterId: Option[UUID],
    beforeId: Option[UUID]) derives Encoder.AsObject, Decoder

final case class BulkUpdateReq(taskIds: List[UUID], patch: Json) derives Encoder.AsObject, Decoder

final case class BoardDto(sprint: Option[SprintDto], tasks: List[TaskDto]) derives Encoder.AsObject, Decoder
final case class BacklogDto(sprints: List[SprintDto], tasks: List[TaskDto]) derives Encoder.AsObject, Decoder
final case class GanttDto(tasks: List[TaskDto], dependencies: List[DependencyDto], milestones: List[MilestoneDto]) derives Encoder.AsObject, Decoder

/** ガントチャートの目印（リリース日など. タスクには紐付かない）. date は yyyy-MM-dd. */
final case class MilestoneDto(id: UUID, name: String, date: String, color: String) derives Encoder.AsObject, Decoder
final case class MilestoneReq(name: String, date: String, color: Option[String]) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ コメント・作業時間

final case class CommentDto(id: UUID, taskId: UUID, author: Option[UserBrief], body: Json, html: String, via: String,
    createdAt: Timestamp, updatedAt: Timestamp, reactions: List[ReactionDto]) derives Encoder.AsObject, Decoder
final case class CommentReq(body: Json) derives Encoder.AsObject, Decoder

/** コメントへのリアクション（絵文字と、付けた人）. */
final case class ReactionDto(emoji: String, users: List[UserBrief]) derives Encoder.AsObject, Decoder
final case class ReactionReq(emoji: String) derives Encoder.AsObject, Decoder
object Reactions {
  /** 付けられる絵文字（分類ごと）. カスタム絵文字は ":名前:" で付ける. */
  val categories: List[(String, List[String])] = List(
    "よく使う" -> List("👍", "❤️", "😄", "🎉", "🙏", "👀", "🚀", "✅", "🤔", "😢", "👏", "🔥", "💯", "🙌", "👌", "✨"),
    "顔" -> List("😀", "😃", "😁", "😆", "😅", "🤣", "😂", "🙂", "😉", "😊", "😇", "🥰", "😍", "🤩", "😘", "😋", "😛", "😜", "🤪", "🤗", "🤭",
      "🤫", "🤐", "😐", "😑", "😶", "😏", "😒", "🙄", "😬", "😌", "😔", "😪", "😴", "😷", "🤒", "🥵", "🥶", "😵", "🤯", "🥳", "😎", "🤓",
      "🧐", "😕", "😟", "🙁", "😮", "😯", "😲", "😳", "🥺", "😦", "😨", "😰", "😥", "😭", "😱", "😖", "😣", "😞", "😓", "😩", "😫", "🥱",
      "😤", "😡", "😠", "🤬", "😈", "💀", "🤡", "👻", "🤖"),
    "手" -> List("👋", "✋", "🖐️", "👌", "🤌", "✌️", "🤞", "🤟", "🤘", "🤙", "👈", "👉", "👆", "👇", "☝️", "👍", "👎", "✊", "👊", "🤛", "🤜",
      "👏", "🙌", "👐", "🤲", "🤝", "🙏", "✍️", "💪", "🫡", "🫶"),
    "記号" -> List("❤️", "🧡", "💛", "💚", "💙", "💜", "🖤", "🤍", "💔", "❣️", "💕", "💯", "💢", "💥", "💫", "💦", "💨", "💬", "💭", "💤",
      "✅", "☑️", "✔️", "❌", "❎", "⭕", "❗", "❓", "⁉️", "‼️", "⚠️", "🚫", "⛔", "🆗", "🆕", "🆙", "🔴", "🟠", "🟡", "🟢", "🔵", "🟣",
      "⬆️", "⬇️", "➡️", "⬅️", "🔁", "➕", "➖"),
    "もの" -> List("🎉", "🎊", "🎈", "🎁", "🏆", "🥇", "🏅", "⭐", "🌟", "✨", "🔥", "⚡", "💡", "📌", "📎", "📝", "📅", "⏰", "⏳", "🔒",
      "🔑", "🛠️", "🔧", "⚙️", "🧪", "🐛", "🚀", "🛸", "💻", "📱", "📦", "📈", "📉", "📊", "💰", "☕", "🍵", "🍺", "🍣", "🍙", "🍰", "🍕"),
    "自然" -> List("🌸", "🌻", "🍀", "🌱", "🌈", "☀️", "🌙", "⛅", "☔", "❄️", "🌊", "🐶", "🐱", "🐼", "🐧", "🐢", "🦄", "🐝", "🦋")
  )

  val all: List[String] = categories.flatMap(_._2).distinct

  /** カスタム絵文字（:名前:）の名前. */
  def customName(e: String): Option[String] = Option(e).filter(_.matches("^:[a-z0-9_+-]{2,32}:$")).map(_.drop(1).dropRight(1))
}

/** カスタム絵文字. url は画像（ログインが必要）. */
final case class CustomEmojiDto(name: String, url: String, createdBy: Option[String]) derives Encoder.AsObject, Decoder
object CustomEmoji {
  def url(name: String): String = s"/api/emoji/$name/image"
}
/** コメントの下書き（ユーザー＋タスクごと）. */
final case class DraftDto(body: Json, updatedAt: Timestamp) derives Encoder.AsObject, Decoder

final case class WorklogDto(id: UUID, taskId: UUID, taskKey: String, taskTitle: String, projectId: UUID, user: UserBrief,
    workDate: String, minutes: Int, note: String, createdAt: Timestamp) derives Encoder.AsObject, Decoder
final case class WorklogReq(workDate: String, minutes: Int, note: Option[String]) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ 変更履歴・同期

final case class EventDto(id: Long, projectId: UUID, taskId: Option[UUID], taskKey: Option[String], actor: Option[UserBrief],
    via: String, kind: String, detail: Json, at: Timestamp) derives Encoder.AsObject, Decoder
final case class ChangesDto(events: List[EventDto], latest: Long) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ 通知

final case class NotificationDto(id: UUID, kind: String, projectKey: Option[String], taskId: Option[UUID], taskKey: Option[String],
    commentId: Option[UUID], actor: Option[UserBrief], title: String, body: String, at: Timestamp, read: Boolean)
    derives Encoder.AsObject, Decoder
final case class NotificationListDto(items: List[NotificationDto], unread: Int) derives Encoder.AsObject, Decoder
final case class MarkReadReq(ids: List[UUID]) derives Encoder.AsObject, Decoder
final case class UnreadDto(unread: Int, mentions: Int) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ テンプレート・繰り返しタスク

/** タスクのテンプレート. title の {{date}}・{{month}} は作成日（2026-11-15・2026-11）に置き換える.
  * personal は個人のテンプレート（どのプロジェクトでも使える. ステータス・担当者・ラベル・カスタムフィールドは持たない）. */
final case class TemplateDto(id: UUID, name: String, title: String, description: Option[Json], statusId: Option[UUID], assigneeId: Option[UUID],
    labelIds: List[UUID], fields: Map[String, Json], subtasks: List[String], dueInDays: Option[Int], priority: Option[String],
    taskType: Option[String], personal: Boolean) derives Encoder.AsObject, Decoder
final case class TemplateReq(name: String, title: String, description: Option[Json] = None, statusId: Option[UUID] = None,
    assigneeId: Option[UUID] = None, labelIds: List[UUID] = Nil, fields: Map[String, Json] = Map.empty, subtasks: List[String] = Nil,
    dueInDays: Option[Int] = None, priority: Option[String] = None, taskType: Option[String] = None) derives Encoder.AsObject {
  /** 個人のテンプレートでは、プロジェクトに属する値（ステータス・担当者・ラベル・カスタムフィールド）を外す. */
  def personal: TemplateReq = copy(statusId = None, assigneeId = None, labelIds = Nil, fields = Map.empty)
}
object TemplateReq {
  private given Configuration = Configuration.default.withDefaults
  given Decoder[TemplateReq] = ConfiguredDecoder.derived[TemplateReq]
}

object Frequency {
  val Daily = "daily"
  val Weekly = "weekly"
  val Monthly = "monthly"
  val all: List[String] = List(Daily, Weekly, Monthly)
  def label(f: String): String = f match {
    case Daily   => "毎日"
    case Weekly  => "毎週"
    case Monthly => "毎月"
    case _       => f
  }
  val weekdays: List[String] = List("月", "火", "水", "木", "金", "土", "日")
}

/** 繰り返しタスク（テンプレートから決まった日に作る）. weekday は 1=月〜7=日、nextRun は yyyy-MM-dd. */
final case class RecurringDto(id: UUID, templateId: UUID, templateName: String, frequency: String, weekday: Option[Int], monthday: Option[Int],
    activeSprint: Boolean, nextRun: String, enabled: Boolean, lastRunAt: Option[Timestamp], lastTaskKey: Option[String])
    derives Encoder.AsObject, Decoder
final case class RecurringReq(templateId: UUID, frequency: String, weekday: Option[Int], monthday: Option[Int], activeSprint: Boolean,
    enabled: Boolean, startDate: Option[String]) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ 取り込み（CSV・Jira）

/** 取り込む1行. ref は元のID（Jira の課題キーなど. parentRef で親子をつなぐ）. 名前・ラベル・スプリントは名前で探す. */
final case class ImportRow(title: String, ref: Option[String] = None, description: Option[String] = None, status: Option[String] = None,
    assignee: Option[String] = None, reporter: Option[String] = None, labels: List[String] = Nil, startDate: Option[String] = None,
    dueDate: Option[String] = None, parentRef: Option[String] = None, sprint: Option[String] = None, priority: Option[String] = None,
    taskType: Option[String] = None) derives Encoder.AsObject
object ImportRow {
  private given Configuration = Configuration.default.withDefaults
  given Decoder[ImportRow] = ConfiguredDecoder.derived[ImportRow]
}
/** dryRun なら作らずに確認だけする. */
final case class ImportReq(rows: List[ImportRow], dryRun: Boolean) derives Encoder.AsObject, Decoder
final case class ImportResultDto(created: Int, keys: Map[String, String], warnings: List[String]) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ 検索

final case class SearchHitDto(task: TaskDto, projectKey: String, snippet: Option[String], fuzzy: Boolean) derives Encoder.AsObject, Decoder
final case class SearchResultDto(hits: List[SearchHitDto], total: Int, error: Option[String]) derives Encoder.AsObject, Decoder
final case class SavedFilterDto(id: UUID, name: String, query: String) derives Encoder.AsObject, Decoder
final case class SavedFilterReq(name: String, query: String) derives Encoder.AsObject, Decoder

/** ボードの絞り込み条件（空文字はその条件なし）. who は "me"・"none"・ユーザーID、label はラベルID、due は overdue・today・week・none. */
final case class BoardCond(q: String = "", who: String = "", label: String = "", priority: String = "", taskType: String = "", due: String = "")
    derives Encoder.AsObject {
  def isEmpty: Boolean = this == BoardCond()
}
object BoardCond {
  private given Configuration = Configuration.default.withDefaults
  given Decoder[BoardCond] = ConfiguredDecoder.derived[BoardCond]
  val dues: List[(String, String)] = List("overdue" -> "期限切れ", "today" -> "今日まで", "week" -> "7日以内", "none" -> "期日なし")
}
/** 保存したボードの絞り込み. shared はプロジェクトで共有, mine は自分が作ったもの（消せる）. */
final case class BoardFilterDto(id: UUID, name: String, cond: BoardCond, shared: Boolean, mine: Boolean) derives Encoder.AsObject, Decoder
final case class BoardFilterReq(name: String, cond: BoardCond, shared: Boolean) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ Webhook

final case class WebhookDto(id: UUID, name: String, url: String, secret: String, events: List[String], enabled: Boolean)
    derives Encoder.AsObject, Decoder
final case class WebhookReq(name: String, url: String, events: List[String], enabled: Boolean) derives Encoder.AsObject, Decoder
final case class DeliveryDto(id: UUID, event: String, attempts: Int, statusCode: Option[Int], lastError: Option[String],
    doneAt: Option[Timestamp], failed: Boolean, createdAt: Timestamp, payload: Json) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ 自動化

final case class RuleDto(id: UUID, name: String, enabled: Boolean, trigger: Json, actions: Json, runAs: Option[UserBrief],
    incomingUrl: Option[String], lastRunAt: Option[Timestamp], createdAt: Timestamp) derives Encoder.AsObject, Decoder
final case class RuleReq(name: String, enabled: Boolean, trigger: Json, actions: Json, runAs: Option[UUID]) derives Encoder.AsObject, Decoder
final case class RunDto(id: UUID, ruleId: UUID, ruleName: String, taskKey: Option[String], status: String, log: Json,
    startedAt: Timestamp, endedAt: Option[Timestamp]) derives Encoder.AsObject, Decoder
/** 手動トリガーのルール（タスク画面のボタン）. */
final case class ManualRuleDto(id: UUID, name: String) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ SCM

/** allRepos なら、サービス全体（組織・グループ・サーバー）の Webhook ですべてのリポジトリを監視する（name は組織名などの絞り込み. 空ならすべて）. */
final case class RepoDto(id: UUID, provider: String, name: String, webUrl: String, apiUrl: Option[String], branch: Option[String],
    hookUrl: String, secret: String, hasToken: Boolean, lastPolledAt: Option[Timestamp], allRepos: Boolean) derives Encoder.AsObject, Decoder
final case class RepoReq(provider: String, name: String, webUrl: String, apiUrl: Option[String], branch: Option[String],
    token: Option[String], allRepos: Option[Boolean]) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ 運用

final case class AuditDto(id: Long, at: Timestamp, actor: Option[UserBrief], via: String, action: String, target: Option[String],
    detail: Json) derives Encoder.AsObject, Decoder

final case class ClientConfigDto(devLogin: Boolean, version: String, attachments: Boolean, mail: Boolean, linkServices: List[String])
    derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ 外部サービスのリンク（skiwi のページ・skawau のファイルの名前を出す）

/** 管理者が設定したサービス. provider は skiwi・skawau、connected は自分が許可したか. */
final case class LinkServiceDto(provider: String, name: String, url: String, connected: Boolean) derives Encoder.AsObject, Decoder
/** リンク先の名前（ページのタイトル・ファイル名）と、前に付けるアイコン. */
final case class LinkTitleDto(id: UUID, title: String, icon: Option[String]) derives Encoder.AsObject, Decoder
/** 見られるものだけ. connected が false なら連携が切れている. */
final case class LinkTitlesDto(items: List[LinkTitleDto], connected: Boolean) derives Encoder.AsObject, Decoder
final case class LinkTitlesReq(ids: List[UUID]) derives Encoder.AsObject, Decoder
