-- ヘルプデスク: プロジェクトのメンバーを、問い合わせする"ユーザ"（requester）と、回答する"エージェント"（view / edit / admin）に分ける

alter table project add column helpdesk boolean not null default false;
-- 窓口（/help/KEY）から OIDC でログインした人が、自分で"ユーザ"として参加できるか。guest_domains が空ならどのメールアドレスでもよい
alter table project add column guest_signup boolean not null default false;
alter table project add column guest_domains text[] not null default '{}';

alter table project_member drop constraint project_member_role_check;
alter table project_member add constraint project_member_role_check check (role in ('requester', 'view', 'edit', 'admin'));
alter table project_group_member drop constraint project_group_member_role_check;
alter table project_group_member add constraint project_group_member_role_check check (role in ('requester', 'view', 'edit', 'admin'));

-- エージェントだけに見える内部コメント
alter table comment add column internal boolean not null default false;

-- "ユーザ"のグループ（プロジェクトごと。親子の階層。親に共有したタスクは子のメンバーにも見える）
create table requester_group (
  id         uuid primary key,
  project_id uuid not null references project(id) on delete cascade,
  parent_id  uuid references requester_group(id) on delete cascade,
  name       text not null,
  created_at timestamptz not null default now(),
  unique (project_id, name),
  check (parent_id <> id)
);
create index requester_group_parent_idx on requester_group (parent_id);

create table requester_group_member (
  group_id uuid not null references requester_group(id) on delete cascade,
  user_id  uuid not null references app_user(id) on delete cascade,
  primary key (group_id, user_id)
);
create index requester_group_member_user_idx on requester_group_member (user_id);

-- タスクの共有先（"ユーザ"のグループか個人のどちらか）
create table task_share (
  task_id    uuid not null references task(id) on delete cascade,
  group_id   uuid references requester_group(id) on delete cascade,
  user_id    uuid references app_user(id) on delete cascade,
  created_by uuid references app_user(id) on delete set null,
  created_at timestamptz not null default now(),
  check (num_nonnulls(group_id, user_id) = 1)
);
create unique index task_share_group_idx on task_share (task_id, group_id) where group_id is not null;
create unique index task_share_user_idx on task_share (task_id, user_id) where user_id is not null;
create index task_share_group_ref_idx on task_share (group_id) where group_id is not null;
create index task_share_user_ref_idx on task_share (user_id) where user_id is not null;
