package skea.api

/** REST API の一覧（MCP の list_api_endpoints と GET /api/endpoints）。 */
object ApiCatalog {

  final case class Endpoint(
    method: String,
    path: String,
    description: String)
  val all: List[Endpoint] = List(
    Endpoint("GET", "/api/me", "自分の情報"),
    Endpoint("PATCH", "/api/me", "自分の名前・タイムゾーンを変える {name?, timezone?}"),
    Endpoint("GET", "/api/me/notify-prefs", "通知の種類ごとのメール送信の設定"),
    Endpoint("PUT", "/api/me/notify-prefs", "通知のメール設定を変える [{kind, email}]"),
    Endpoint("GET", "/api/users?q=", "ユーザーを探す"),
    Endpoint("GET", "/api/projects", "見られるプロジェクトの一覧（?archived=true でアーカイブも）"),
    Endpoint("POST", "/api/projects", "プロジェクトを作る（管理者） {key, name, description?}"),
    Endpoint("GET", "/api/projects/by-key/{key}", "キーでプロジェクトを取得"),
    Endpoint("GET", "/api/projects/{projectId}/meta", "プロジェクトの設定一式（ステータス・ラベル・スプリント・カスタムフィールド・メンバー）"),
    Endpoint("PATCH", "/api/projects/{projectId}", "プロジェクトを変える {name?, description?, leadId?, archived?}"),
    Endpoint("DELETE", "/api/projects/{projectId}", "プロジェクトを削除する（管理者）"),
    Endpoint("GET", "/api/projects/{projectId}/members", "メンバーと権限"),
    Endpoint("PUT", "/api/projects/{projectId}/members", "メンバーの権限を設定 {userId, role: view|edit|admin}"),
    Endpoint("DELETE", "/api/projects/{projectId}/members/{userId}", "メンバーを外す"),
    Endpoint("GET", "/api/projects/{projectId}/groups", "グループの権限"),
    Endpoint("PUT", "/api/projects/{projectId}/groups", "グループの権限を設定 {groupName, role}"),
    Endpoint("GET", "/api/projects/{projectId}/audience", "プロジェクトを閲覧できるユーザー（担当者の候補）"),
    Endpoint(
      "POST",
      "/api/projects/{projectId}/statuses",
      "ステータスを追加 {name, category: todo|in_progress|waiting|done, color?}"
    ),
    Endpoint("PUT", "/api/projects/{projectId}/statuses/{statusId}", "ステータスを変える"),
    Endpoint("PUT", "/api/projects/{projectId}/statuses/order", "ステータスの並び順 {ids}"),
    Endpoint("DELETE", "/api/projects/{projectId}/statuses/{statusId}?moveTo={statusId}", "ステータスを削除（タスクは moveTo へ）"),
    Endpoint("POST", "/api/projects/{projectId}/labels", "ラベルを追加 {name, color?}"),
    Endpoint("PUT", "/api/projects/{projectId}/labels/{labelId}", "ラベルを変える"),
    Endpoint("DELETE", "/api/projects/{projectId}/labels/{labelId}", "ラベルを削除"),
    Endpoint(
      "POST",
      "/api/projects/{projectId}/fields",
      "カスタムフィールドを追加 {name, kind: text|textarea|checkbox|radio|select, options, required}"
    ),
    Endpoint("PUT", "/api/projects/{projectId}/fields/{fieldId}", "カスタムフィールドを変える"),
    Endpoint("DELETE", "/api/projects/{projectId}/fields/{fieldId}", "カスタムフィールドを削除"),
    Endpoint("GET", "/api/projects/{projectId}/sprints?closed=true", "スプリントの一覧"),
    Endpoint("POST", "/api/projects/{projectId}/sprints", "スプリントを作る {name, goal?, startDate?, endDate?}"),
    Endpoint("PUT", "/api/projects/{projectId}/sprints/{sprintId}", "スプリントを変える"),
    Endpoint("POST", "/api/projects/{projectId}/sprints/{sprintId}/start", "スプリントを開始"),
    Endpoint(
      "POST",
      "/api/projects/{projectId}/sprints/{sprintId}/complete",
      "スプリントを完了 {moveTo?: 未完了タスクの移動先スプリントID（なければバックログ）}"
    ),
    Endpoint("DELETE", "/api/projects/{projectId}/sprints/{sprintId}", "スプリントを削除"),
    Endpoint("GET", "/api/projects/{projectId}/board", "カンバン（アクティブなスプリントのタスク）"),
    Endpoint("GET", "/api/projects/{projectId}/backlog", "バックログ（スプリントとタスク）"),
    Endpoint("GET", "/api/projects/{projectId}/gantt?done=true", "ガントチャート（タスク・依存関係・マイルストーン）"),
    Endpoint("POST", "/api/projects/{projectId}/milestones", "マイルストーン（ガントの目印）を追加 {name, date, color?}"),
    Endpoint("PUT", "/api/projects/{projectId}/milestones/{milestoneId}", "マイルストーンを変更 {name, date, color?}"),
    Endpoint("DELETE", "/api/projects/{projectId}/milestones/{milestoneId}", "マイルストーンを削除"),
    Endpoint("GET", "/api/projects/{projectId}/changes?since={eventId}", "変更の取得（画面の同期）"),
    Endpoint("GET", "/api/projects/{projectId}/activity", "最近の活動"),
    Endpoint(
      "POST",
      "/api/projects/{projectId}/tasks",
      "タスクを作る {title, description?, statusId?, sprintId?, assigneeId?, parentId?, startDate?, dueDate?, labelIds?, fields?, priority?, taskType?}"
    ),
    Endpoint("GET", "/api/tasks/{taskIdOrKey}", "タスクの詳細"),
    Endpoint("POST", "/api/tasks/lookup", "タスクIDのタイトルと状態をまとめて読む {keys: [SKEA-1, …]}（100件まで）"),
    Endpoint(
      "PATCH",
      "/api/tasks/{taskId}",
      "タスクを変える（変えるキーだけ。title, description, statusId, sprintId, assigneeId, reporterId, parentId, startDate, dueDate, labelIds, fields, priority, taskType, version）"
    ),
    Endpoint("DELETE", "/api/tasks/{taskId}", "タスクを削除（サブタスクも）"),
    Endpoint(
      "POST",
      "/api/tasks/{taskId}/move",
      "カンバン・バックログでの移動 {statusId?, sprintId?, toBacklog?, afterId?, beforeId?}"
    ),
    Endpoint("POST", "/api/tasks/bulk", "一括更新 {taskIds, patch}"),
    Endpoint("GET", "/api/tasks/{taskId}/history", "変更履歴"),
    Endpoint("POST", "/api/tasks/{taskId}/watch", "ウォッチする（DELETE で解除）"),
    Endpoint("POST", "/api/tasks/{taskId}/watchers", "ウォッチャーを追加 {userId}"),
    Endpoint("POST", "/api/tasks/{taskId}/links", "リンクを追加 {kind, targetKey?} または {url, title?}"),
    Endpoint("DELETE", "/api/tasks/{taskId}/links/{linkId}", "リンクを削除"),
    Endpoint("POST", "/api/tasks/{taskId}/dependencies", "先行タスクを設定（ガント） {predecessorKey, kind?: fs|ss|ff|sf}"),
    Endpoint("DELETE", "/api/tasks/{taskId}/dependencies/{predecessorId}", "先行タスクを外す"),
    Endpoint("GET", "/api/tasks/{taskId}/worklogs", "作業時間"),
    Endpoint("POST", "/api/tasks/{taskId}/worklogs", "作業時間を記録 {workDate, minutes, note?}"),
    Endpoint("PUT", "/api/worklogs/{worklogId}", "作業時間を変える"),
    Endpoint("DELETE", "/api/worklogs/{worklogId}", "作業時間を削除"),
    Endpoint("GET", "/api/worklogs?from=&to=&projectId=&userId=", "作業時間の一覧（日次の集計用）"),
    Endpoint("GET", "/api/tasks/{taskId}/comments", "コメント"),
    Endpoint("POST", "/api/tasks/{taskId}/comments", "コメントを追加 {body: Tiptap の文書 または Markdown の文字列}"),
    Endpoint("POST", "/api/comments/{commentId}/reactions", "コメントにリアクション {emoji}（👍 ❤️ 😄 🎉 🙏 👀 🚀 ✅ 🤔 😢 👏 🔥）"),
    Endpoint("DELETE", "/api/comments/{commentId}/reactions?emoji={emoji}", "リアクションを外す"),
    Endpoint("GET", "/api/projects/{projectId}/templates", "タスクのテンプレート"),
    Endpoint(
      "POST",
      "/api/projects/{projectId}/templates",
      "テンプレートを作る {name, title, description?, statusId?, assigneeId?, labelIds?, fields?, subtasks?: [タイトル], dueInDays?, priority?, taskType?}"
    ),
    Endpoint("GET", "/api/me/templates", "自分用（個人）のテンプレート"),
    Endpoint(
      "POST",
      "/api/me/templates",
      "自分用のテンプレートを作る {name, title, description?, assigneeId?, subtasks?, dueInDays?, priority?, taskType?}"
    ),
    Endpoint(
      "POST",
      "/api/templates/{templateId}/tasks?activeSprint=true&projectId=…",
      "テンプレートからタスク（とサブタスク）を作る（自分用のテンプレートは projectId で作成先を指定）"
    ),
    Endpoint("GET", "/api/projects/{projectId}/recurring", "繰り返しタスク"),
    Endpoint(
      "POST",
      "/api/projects/{projectId}/recurring",
      "繰り返しタスクを追加 {templateId, frequency: daily|weekly|monthly, weekday?: 1-7, monthday?: 1-31, activeSprint, enabled, startDate?}"
    ),
    Endpoint(
      "POST",
      "/api/projects/{projectId}/import",
      "タスクの取り込み {rows: [{title, ref?, parentRef?, description?, status?, assignee?, reporter?, labels?, startDate?, dueDate?, sprint?}], dryRun}"
    ),
    Endpoint("PUT", "/api/comments/{commentId}", "コメントを編集"),
    Endpoint("DELETE", "/api/comments/{commentId}", "コメントを削除"),
    Endpoint("GET", "/api/tasks/{taskId}/attachments", "添付ファイル"),
    Endpoint("POST", "/api/tasks/{taskId}/attachments", "添付ファイルを追加（multipart/form-data の file）"),
    Endpoint("DELETE", "/api/attachments/{attachmentId}", "添付ファイルを削除"),
    Endpoint("GET", "/api/tasks/{taskId}/automations", "手動で実行できる自動化のルール"),
    Endpoint("POST", "/api/tasks/{taskId}/automations/{ruleId}/run", "自動化のルールを手動で実行"),
    Endpoint(
      "GET",
      "/api/search?q=&projectId=&limit=&offset=",
      "タスクの検索（絞り込み構文: project: status: assignee:me label: sprint:active due< is:open has: field:名前=値 order:-updated）"
    ),
    Endpoint("GET", "/api/filters", "保存した検索条件"),
    Endpoint("POST", "/api/filters", "検索条件を保存 {name, query}"),
    Endpoint("GET", "/api/notifications?kinds=mention&unread=true", "通知の一覧"),
    Endpoint("POST", "/api/notifications/read", "通知を既読にする {ids}"),
    Endpoint("POST", "/api/notifications/read-all", "通知をすべて既読にする"),
    Endpoint("GET", "/api/projects/{projectId}/webhooks", "Webhook（プロジェクト管理者）"),
    Endpoint("GET", "/api/projects/{projectId}/webhooks/sample?event={event}", "Webhook で送る JSON の見本"),
    Endpoint("POST", "/api/projects/{projectId}/webhooks", "Webhook を追加 {name, url, events, enabled}"),
    Endpoint("GET", "/api/projects/{projectId}/automations", "自動化のルール（プロジェクト管理者）"),
    Endpoint("POST", "/api/projects/{projectId}/automations", "自動化のルールを追加 {name, enabled, trigger, actions, runAs?}"),
    Endpoint("PUT", "/api/automations/{ruleId}", "自動化のルールを変える"),
    Endpoint("GET", "/api/projects/{projectId}/automation-runs", "自動化の実行結果"),
    Endpoint("GET", "/api/projects/{projectId}/repos", "連携しているリポジトリ"),
    Endpoint("POST", "/api/projects/{projectId}/repos", "リポジトリを追加 {provider, name, webUrl, apiUrl?, branch?, token?}"),
    Endpoint("POST", "/api/repos/{repoId}/poll", "リポジトリのコミットを今すぐ取得"),
    Endpoint("GET", "/api/admin/users", "ユーザーの一覧（管理者）"),
    Endpoint("PATCH", "/api/admin/users/{userId}", "ユーザーを変える（管理者） {isAdmin?, disabled?}"),
    Endpoint("GET", "/api/admin/agents", "エージェント専用アカウントとキーの一覧（管理者・画面からのみ）"),
    Endpoint("POST", "/api/admin/agents", "エージェント専用アカウントを作る（管理者・画面からのみ） {name}"),
    Endpoint("PATCH", "/api/admin/agents/{agentId}", "エージェント専用アカウントの名前の変更・無効化（管理者・画面からのみ） {name?, disabled?}"),
    Endpoint(
      "POST",
      "/api/admin/agents/{agentId}/keys",
      "エージェント専用アカウントのキーを発行（管理者・画面からのみ。admin スコープは不可） {name, scopes, projectIds?, expiresInDays?}"
    ),
    Endpoint("DELETE", "/api/admin/agents/{agentId}/keys/{keyId}", "エージェント専用アカウントのキーを失効（管理者・画面からのみ）"),
    Endpoint("GET", "/api/admin/audit", "監査ログ（管理者）")
  )
}
