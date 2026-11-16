-- タスクのテンプレート: プロジェクトのもの（project_id）と、ユーザー個人のもの（user_id）に分ける。優先度・種類も持つ
alter table task_template alter column project_id drop not null;
alter table task_template add column user_id uuid references app_user(id) on delete cascade;
alter table task_template add column priority text;
alter table task_template add column task_type text;
alter table task_template add constraint task_template_owner check (num_nonnulls(project_id, user_id) = 1);
create unique index task_template_user_name on task_template (user_id, name) where user_id is not null;

-- ボードの絞り込み条件（名前を付けて保存する。user_id が空ならプロジェクトで共有する）
create table board_filter (
  id         uuid primary key,
  project_id uuid not null references project(id) on delete cascade,
  user_id    uuid references app_user(id) on delete cascade,
  name       text not null,
  cond       jsonb not null,
  created_by uuid references app_user(id) on delete set null,
  created_at timestamptz not null default now()
);
create index board_filter_project_idx on board_filter (project_id);
create unique index board_filter_shared_name on board_filter (project_id, name) where user_id is null;
create unique index board_filter_user_name on board_filter (project_id, user_id, name) where user_id is not null;
