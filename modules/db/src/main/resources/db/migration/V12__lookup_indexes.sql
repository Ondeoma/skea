-- よく使う検索に足りていなかったインデックスを足す。

-- 認証のたびに、ユーザーの所属グループを引く（主キーは (group_id, user_id) なので user_id からは引けない）
create index group_member_user_idx on group_member (user_id);
-- 参加しているプロジェクトの一覧で、グループから権限を引く（主キーは (project_id, group_id)）
create index project_group_member_group_idx on project_group_member (group_id);

-- 出来事のたびに、プロジェクトの Webhook を引く
create index webhook_project_idx on webhook (project_id);
-- Webhook ごとの送信の履歴（新しい順）と、Webhook を消したときの連鎖の削除
create index webhook_delivery_webhook_idx on webhook_delivery (webhook_id, created_at desc);

-- プロジェクトのリポジトリの一覧と、リポジトリを消したときのコミットの連鎖の削除
create index scm_repo_project_idx on scm_repo (project_id);
create index task_commit_repo_idx on task_commit (repo_id);

-- ヘルプデスクの窓口: 自分が問い合わせたタスク
create index task_reporter_idx on task (reporter_id) where deleted_at is null;
-- ステータスを消すときのタスクの付け替えと、外部キーの確認
create index task_status_idx on task (status_id);

-- 保存した検索条件の一覧
create index saved_filter_user_idx on saved_filter (user_id);
