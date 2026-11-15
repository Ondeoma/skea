-- skea のスキーマ
-- 適用済みのマイグレーションは書き換えないこと。変更は新しい番号の V<n>__*.sql を追加する。

create extension if not exists pgroonga;

-- ------------------------------------------------------------------ ユーザー・認証

create table app_user (
  id            uuid primary key,
  sub           text not null unique,            -- OIDC の sub
  name          text not null,
  email         text,
  avatar_url    text,
  avatar_key    text,                            -- 画面から登録したアイコン（S3 のキー）。登録している間は IdP のアイコンで上書きしない
  is_admin      boolean not null default false,  -- 画面で付けた管理者権限（ADMIN_GROUPS の所属者は別途管理者）
  disabled      boolean not null default false,
  timezone      text not null default 'Asia/Tokyo',
  created_at    timestamptz not null default now(),
  last_login_at timestamptz
);
create index app_user_name_idx on app_user using pgroonga (name);

create table user_group (
  id   uuid primary key,
  name text not null unique
);

create table group_member (
  group_id uuid not null references user_group(id) on delete cascade,
  user_id  uuid not null references app_user(id) on delete cascade,
  primary key (group_id, user_id)
);

create table session (
  token_hash bytea primary key,
  user_id    uuid not null references app_user(id) on delete cascade,
  created_at timestamptz not null default now(),
  expires_at timestamptz not null
);
create index session_expires_idx on session (expires_at);

-- OIDC のログイン途中の状態（複数ノードのどこに戻ってきてもよいよう DB に置く）
create table login_state (
  state      text primary key,
  verifier   text not null,
  nonce      text not null,
  return_to  text not null default '/',
  created_at timestamptz not null default now()
);

create table api_key (
  id           uuid primary key,
  user_id      uuid not null references app_user(id) on delete cascade,
  name         text not null,
  prefix       text not null,                    -- 画面に出す先頭部分（skea_xxxx）
  key_hash     bytea not null unique,
  scopes       text[] not null,                  -- read / write / admin
  project_ids  uuid[],                           -- null なら本人が見られるすべて
  created_at   timestamptz not null default now(),
  expires_at   timestamptz,
  last_used_at timestamptz,
  revoked_at   timestamptz,
  oauth_client text                              -- 外部サービスへの認可で発行したキーのクライアント（許可し直したら前のキーを失効させる）
);
create index api_key_user_idx on api_key (user_id);
create index api_key_oauth_idx on api_key (user_id, oauth_client) where oauth_client is not null;

-- ------------------------------------------------------------------ プロジェクト

create table project (
  id          uuid primary key,
  key         text not null unique check (key ~ '^[A-Z][A-Z0-9]{1,9}$'),
  name        text not null,
  description text not null default '',
  lead_id     uuid references app_user(id),
  archived    boolean not null default false,
  task_seq    bigint not null default 0,
  position    text not null default 'a0',
  created_at  timestamptz not null default now()
);

create table project_member (
  project_id uuid not null references project(id) on delete cascade,
  user_id    uuid not null references app_user(id) on delete cascade,
  role       text not null check (role in ('view', 'edit', 'admin')),
  primary key (project_id, user_id)
);
create index project_member_user_idx on project_member (user_id);

-- グループ単位の権限（IdP のグループに付ける）
create table project_group_member (
  project_id uuid not null references project(id) on delete cascade,
  group_id   uuid not null references user_group(id) on delete cascade,
  role       text not null check (role in ('view', 'edit', 'admin')),
  primary key (project_id, group_id)
);

create table status (
  id         uuid primary key,
  project_id uuid not null references project(id) on delete cascade,
  name       text not null,
  category   text not null check (category in ('todo', 'in_progress', 'waiting', 'done')),
  color      text not null default '#64748b',
  position   int not null,
  unique (project_id, name)
);

create table label (
  id         uuid primary key,
  project_id uuid not null references project(id) on delete cascade,
  name       text not null,
  color      text not null default '#64748b',
  unique (project_id, name)
);

create table sprint (
  id           uuid primary key,
  project_id   uuid not null references project(id) on delete cascade,
  name         text not null,
  goal         text not null default '',
  state        text not null default 'planned' check (state in ('planned', 'active', 'closed')),
  start_date   date,
  end_date     date,
  position     int not null default 0,
  completed_at timestamptz,
  created_at   timestamptz not null default now()
);
-- アクティブなスプリントは1プロジェクトに1つ
create unique index sprint_one_active_idx on sprint (project_id) where state = 'active';

-- ガントチャートの目印（リリース日など。タスクには紐付かない）
create table milestone (
  id         uuid primary key,
  project_id uuid not null references project(id) on delete cascade,
  name       text not null,
  date       date not null,
  color      text not null default '#dc2626',
  created_at timestamptz not null default now()
);
create index milestone_project_idx on milestone (project_id, date);

create table custom_field (
  id         uuid primary key,
  project_id uuid not null references project(id) on delete cascade,
  name       text not null,
  kind       text not null check (kind in ('text', 'textarea', 'checkbox', 'radio', 'select')),
  options    jsonb not null default '[]',   -- 選択肢（checkbox / radio / select）
  required   boolean not null default false,
  position   int not null default 0,
  unique (project_id, name)
);

-- ------------------------------------------------------------------ タスク

create table task (
  id               uuid primary key,
  project_id       uuid not null references project(id) on delete cascade,
  number           bigint not null,
  key              text not null unique,          -- SKEA-123
  parent_id        uuid references task(id) on delete set null,
  title            text not null,
  description      jsonb,                         -- Tiptap(ProseMirror) の JSON
  description_text text not null default '',     -- 検索用の平文
  status_id        uuid not null references status(id),
  sprint_id        uuid references sprint(id) on delete set null,
  assignee_id      uuid references app_user(id),
  reporter_id      uuid references app_user(id),
  start_date       date,
  due_date         date,
  priority         text not null default 'medium' check (priority in ('highest', 'high', 'medium', 'low', 'lowest')),
  type             text not null default 'task' check (type in ('task', 'bug', 'story', 'epic', 'improvement')),
  rank             text not null,                 -- fractional index（カンバン・バックログの並び順）
  version          int not null default 1,        -- 楽観ロック
  created_at       timestamptz not null default now(),
  updated_at       timestamptz not null default now(),
  resolved_at      timestamptz,
  deleted_at       timestamptz,
  unique (project_id, number)
);
create index task_project_idx on task (project_id) where deleted_at is null;
create index task_sprint_idx on task (sprint_id) where deleted_at is null;
create index task_assignee_idx on task (assignee_id) where deleted_at is null;
create index task_parent_idx on task (parent_id);
create index task_due_idx on task (due_date) where deleted_at is null;
create index task_priority_idx on task (project_id, priority);
create index task_type_idx on task (project_id, type);
create index task_search_idx on task using pgroonga ((array[key, title, description_text]) pgroonga_text_array_full_text_search_ops_v2)
  with (
    tokenizer = 'TokenNgram("unify_alphabet", false, "unify_symbol", false, "unify_digit", false, "loose_symbol", true)',
    normalizers = 'NormalizerNFKC150("unify_kana", true, "unify_kana_case", true, "unify_hyphen_and_prolonged_sound_mark", true, "unify_middle_dot", true, "unify_katakana_v_sounds", true)'
  );

create table task_label (
  task_id  uuid not null references task(id) on delete cascade,
  label_id uuid not null references label(id) on delete cascade,
  primary key (task_id, label_id)
);
create index task_label_label_idx on task_label (label_id);

create table task_watcher (
  task_id uuid not null references task(id) on delete cascade,
  user_id uuid not null references app_user(id) on delete cascade,
  primary key (task_id, user_id)
);
create index task_watcher_user_idx on task_watcher (user_id);

-- リンク: 別のタスク（target_task_id）または任意の URL
create table task_link (
  id             uuid primary key,
  task_id        uuid not null references task(id) on delete cascade,
  kind           text not null,                 -- relates / blocks / is_blocked_by / duplicates / is_duplicated_by / clones / web
  target_task_id uuid references task(id) on delete cascade,
  url            text,
  title          text not null default '',
  created_by     uuid references app_user(id),
  created_at     timestamptz not null default now(),
  check ((target_task_id is not null) <> (url is not null))
);
create index task_link_task_idx on task_link (task_id);
create index task_link_target_idx on task_link (target_task_id);

-- ガントチャートの依存関係（predecessor が終わってから successor を始める）
create table task_dependency (
  predecessor_id uuid not null references task(id) on delete cascade,
  successor_id   uuid not null references task(id) on delete cascade,
  kind           text not null default 'fs' check (kind in ('fs', 'ss', 'ff', 'sf')),
  primary key (predecessor_id, successor_id),
  check (predecessor_id <> successor_id)
);
create index task_dependency_succ_idx on task_dependency (successor_id);

create table task_field_value (
  task_id  uuid not null references task(id) on delete cascade,
  field_id uuid not null references custom_field(id) on delete cascade,
  value    jsonb not null,
  primary key (task_id, field_id)
);

create table worklog (
  id         uuid primary key,
  task_id    uuid not null references task(id) on delete cascade,
  user_id    uuid not null references app_user(id),
  work_date  date not null,
  minutes    int not null check (minutes > 0),
  note       text not null default '',
  created_at timestamptz not null default now()
);
create index worklog_task_idx on worklog (task_id);
create index worklog_user_date_idx on worklog (user_id, work_date);
create index worklog_date_idx on worklog (work_date);

create table comment (
  id         uuid primary key,
  task_id    uuid not null references task(id) on delete cascade,
  author_id  uuid references app_user(id),
  body       jsonb not null,
  body_text  text not null default '',
  via        text not null default 'web',          -- web / api / automation
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  deleted_at timestamptz
);
create index comment_task_idx on comment (task_id, created_at);
create index comment_search_idx on comment using pgroonga (body_text)
  with (
    tokenizer = 'TokenNgram("unify_alphabet", false, "unify_symbol", false, "unify_digit", false, "loose_symbol", true)',
    normalizers = 'NormalizerNFKC150("unify_kana", true, "unify_kana_case", true, "unify_hyphen_and_prolonged_sound_mark", true, "unify_middle_dot", true, "unify_katakana_v_sounds", true)'
  );

-- コメントの下書き（ユーザー＋タスクごとに1つ。投稿したら消す）
create table comment_draft (
  user_id    uuid not null references app_user(id) on delete cascade,
  task_id    uuid not null references task(id) on delete cascade,
  body       jsonb not null,
  updated_at timestamptz not null default now(),
  primary key (user_id, task_id)
);

-- リアクション（決まった絵文字と、:name: のカスタム絵文字）
create table comment_reaction (
  comment_id uuid not null references comment(id) on delete cascade,
  user_id    uuid not null references app_user(id) on delete cascade,
  emoji      text not null,
  created_at timestamptz not null default now(),
  primary key (comment_id, user_id, emoji)
);

-- リアクションに使えるカスタム絵文字（システム全体。:name: で使う）
create table custom_emoji (
  id           uuid primary key,
  name         text not null unique check (name ~ '^[a-z0-9_+-]{2,32}$'),
  s3_key       text not null,
  content_type text not null,
  created_by   uuid references app_user(id) on delete set null,
  created_at   timestamptz not null default now()
);

create table attachment (
  id           uuid primary key,
  task_id      uuid not null references task(id) on delete cascade,
  s3_key       text not null,
  file_name    text not null,
  size         bigint not null,
  content_type text not null,
  sha256       text not null,
  created_by   uuid references app_user(id),
  created_at   timestamptz not null default now()
);
create index attachment_task_idx on attachment (task_id);

-- 変更履歴（画面の自動同期にも使う。id の大きいものが新しい）
create table task_event (
  id         bigserial primary key,
  project_id uuid not null references project(id) on delete cascade,
  task_id    uuid references task(id) on delete cascade,
  actor_id   uuid references app_user(id),
  via        text not null default 'web',          -- web / api / mcp / automation / scm / system
  kind       text not null,                        -- task.created / task.updated / comment.added / sprint.started ...
  detail     jsonb not null default '{}',
  at         timestamptz not null default now()
);
create index task_event_project_idx on task_event (project_id, id);
create index task_event_task_idx on task_event (task_id, id);

-- ------------------------------------------------------------------ テンプレート・繰り返しタスク

-- タスクのテンプレート（タイトル・説明・ラベルなどの初期値と、一緒に作るサブタスク）
create table task_template (
  id          uuid primary key,
  project_id  uuid not null references project(id) on delete cascade,
  name        text not null,
  title       text not null,                      -- {{date}}・{{month}} を作成日に置き換える
  description jsonb,
  status_id   uuid references status(id) on delete set null,
  assignee_id uuid references app_user(id) on delete set null,
  label_ids   uuid[] not null default '{}',
  fields      jsonb not null default '{}',
  subtasks    jsonb not null default '[]',        -- サブタスクのタイトルの配列
  due_in_days int,                                -- 作成日から何日後を期日にするか
  created_by  uuid references app_user(id) on delete set null,
  created_at  timestamptz not null default now(),
  unique (project_id, name)
);

-- 繰り返しタスク（テンプレートから毎日・毎週・毎月作る）
create table recurring_task (
  id            uuid primary key,
  project_id    uuid not null references project(id) on delete cascade,
  template_id   uuid not null references task_template(id) on delete cascade,
  frequency     text not null check (frequency in ('daily', 'weekly', 'monthly')),
  weekday       int check (weekday between 1 and 7),     -- 毎週: 1=月 … 7=日
  monthday      int check (monthday between 1 and 31),   -- 毎月: 日（月末を超えるときは月末）
  active_sprint boolean not null default false,          -- 進行中のスプリントに入れる
  next_run      date not null,
  enabled       boolean not null default true,
  last_run_at   timestamptz,
  last_task_key text,
  created_by    uuid references app_user(id) on delete set null,
  created_at    timestamptz not null default now()
);
create index recurring_task_next_idx on recurring_task (next_run) where enabled;

-- ------------------------------------------------------------------ 通知

create table notification (
  id         uuid primary key,
  user_id    uuid not null references app_user(id) on delete cascade,
  kind       text not null,                        -- mention / assigned / comment / updated / status / due / automation
  project_id uuid references project(id) on delete cascade,
  task_id    uuid references task(id) on delete cascade,
  comment_id uuid references comment(id) on delete cascade,
  actor_id   uuid references app_user(id),
  title      text not null,
  body       text not null default '',
  at         timestamptz not null default now(),
  read_at    timestamptz
);
create index notification_user_idx on notification (user_id, at desc);
create index notification_unread_idx on notification (user_id) where read_at is null;

-- 通知の種類ごとのメール送信の可否（行がなければ送る）
create table notify_pref (
  user_id uuid not null references app_user(id) on delete cascade,
  kind    text not null,
  email   boolean not null,
  primary key (user_id, kind)
);

create table mail_outbox (
  id         uuid primary key,
  user_id    uuid references app_user(id) on delete cascade,
  to_address text,                                 -- user_id がない宛先（自動化のメール送信）
  project_id uuid references project(id) on delete cascade,
  subject    text not null,
  body       text not null,
  dedupe_key text,
  not_before timestamptz not null default now(),
  attempts   int not null default 0,
  last_error text,
  sent_at    timestamptz,
  skipped    boolean not null default false,
  created_at timestamptz not null default now()
);
create index mail_outbox_pending_idx on mail_outbox (not_before) where sent_at is null and not skipped;
create unique index mail_outbox_dedupe_idx on mail_outbox (dedupe_key) where sent_at is null and not skipped and dedupe_key is not null;

-- ------------------------------------------------------------------ Webhook（送信）

create table webhook (
  id         uuid primary key,
  project_id uuid not null references project(id) on delete cascade,
  name       text not null,
  url        text not null,
  secret     text not null,
  events     text[] not null,                      -- 空なら全イベント
  enabled    boolean not null default true,
  created_at timestamptz not null default now()
);

create table webhook_delivery (
  id          uuid primary key,
  webhook_id  uuid not null references webhook(id) on delete cascade,
  event       text not null,
  payload     jsonb not null,
  not_before  timestamptz not null default now(),
  attempts    int not null default 0,
  status_code int,
  last_error  text,
  done_at     timestamptz,
  failed      boolean not null default false,
  created_at  timestamptz not null default now()
);
create index webhook_delivery_pending_idx on webhook_delivery (not_before) where done_at is null and not failed;

-- ------------------------------------------------------------------ 自動化

create table automation_rule (
  id             uuid primary key,
  project_id     uuid not null references project(id) on delete cascade,
  name           text not null,
  enabled        boolean not null default true,
  trigger        jsonb not null,
  actions        jsonb not null,
  run_as         uuid references app_user(id),     -- アクションを実行するユーザー（null なら作成者）
  created_by     uuid references app_user(id),
  incoming_token text unique,                      -- 受信 Webhook トリガーの秘密の値
  last_run_at    timestamptz,                      -- 定期トリガーの前回実行
  created_at     timestamptz not null default now(),
  updated_at     timestamptz not null default now()
);
create index automation_rule_project_idx on automation_rule (project_id);

-- 実行待ち（イベントからルールを起動する）
create table automation_queue (
  id         uuid primary key,
  rule_id    uuid not null references automation_rule(id) on delete cascade,
  task_id    uuid references task(id) on delete cascade,
  actor_id   uuid references app_user(id),
  event      jsonb not null,
  depth      int not null default 0,
  not_before timestamptz not null default now(),
  attempts   int not null default 0,
  done_at    timestamptz,
  created_at timestamptz not null default now()
);
create index automation_queue_pending_idx on automation_queue (not_before) where done_at is null;

create table automation_run (
  id         uuid primary key,
  rule_id    uuid not null references automation_rule(id) on delete cascade,
  task_id    uuid references task(id) on delete set null,
  status     text not null,                        -- success / no_match / error
  log        jsonb not null default '[]',
  started_at timestamptz not null default now(),
  ended_at   timestamptz
);
create index automation_run_rule_idx on automation_run (rule_id, started_at desc);

-- ------------------------------------------------------------------ ソースコード管理（SCM）

create table scm_repo (
  id             uuid primary key,
  project_id     uuid not null references project(id) on delete cascade,
  provider       text not null check (provider in ('github', 'gitlab', 'bitbucket', 'forgejo')),
  name           text not null,                    -- owner/repo
  web_url        text not null,
  api_url        text,                             -- 自前のサーバー（GitLab / Forgejo など）の API の URL
  secret         text not null,                    -- Webhook の署名の秘密
  token_enc      text,                             -- ポーリング用のアクセストークン（暗号化）
  branch         text,
  last_polled_at timestamptz,
  last_sha       text,
  all_repos      boolean not null default false,   -- サービス全体（組織・グループ・サーバー）の Webhook で、すべてのリポジトリの push を受ける
  created_at     timestamptz not null default now()
);

create table task_commit (
  task_id      uuid not null references task(id) on delete cascade,
  repo_id      uuid not null references scm_repo(id) on delete cascade,
  sha          text not null,
  message      text not null,
  url          text not null,
  author       text not null,
  branch       text,
  repo_name    text,                               -- コミットのリポジトリ名（一括監視では、受信するまでリポジトリが決まらない）
  committed_at timestamptz not null,
  primary key (task_id, repo_id, sha)
);

-- ------------------------------------------------------------------ 外部サービスとの連携（OAuth）

-- skea が認可を出す側（skiwi などが skea を読む）。認可コード: 60秒で失効、一度だけ使える。ハッシュだけを保存する
create table oauth_code (
  code_hash      bytea primary key,
  client_id      text not null,
  redirect_uri   text not null,
  user_id        uuid not null references app_user(id) on delete cascade,
  code_challenge text not null,
  scope          text not null,
  expires_at     timestamptz not null,
  used_at        timestamptz
);
create index oauth_code_expires_idx on oauth_code (expires_at);

-- skea が認可を受ける側（skea が skiwi・skawau を読む）。ユーザーごとのトークン
create table integration_token (
  user_id    uuid not null references app_user(id) on delete cascade,
  provider   text not null,                      -- skiwi / skawau
  token_enc  text not null,                      -- APP_SECRET の鍵で暗号化したトークン
  created_at timestamptz not null default now(),
  primary key (user_id, provider)
);

-- 認可を始めたときの state と PKCE の code_verifier（10分で失効。ノードをまたいでも使えるよう DB に置く）
create table integration_state (
  state         text primary key,
  user_id       uuid not null references app_user(id) on delete cascade,
  provider      text not null,
  code_verifier text not null,
  return_to     text not null,
  expires_at    timestamptz not null
);

-- ------------------------------------------------------------------ 運用

-- 複数ノードで1つだけ動かす定期処理のリース
create table job_lock (
  name       text primary key,
  holder     text not null,
  expires_at timestamptz not null
);

create table audit_log (
  id       bigserial primary key,
  at       timestamptz not null default now(),
  actor_id uuid,
  via      text not null,
  action   text not null,
  target   text,
  detail   jsonb not null default '{}'
);
create index audit_log_at_idx on audit_log (at desc);

create function audit_log_append_only() returns trigger language plpgsql as $$
begin
  raise exception 'audit_log is append-only';
end $$;
create trigger audit_log_no_update before update or delete on audit_log
  for each row execute function audit_log_append_only();

-- 保存した検索条件（フィルター）
create table saved_filter (
  id         uuid primary key,
  user_id    uuid not null references app_user(id) on delete cascade,
  name       text not null,
  query      text not null,
  created_at timestamptz not null default now()
);
