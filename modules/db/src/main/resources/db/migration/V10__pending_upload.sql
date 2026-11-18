-- 窓口の「新しい問い合わせ」の内容に、送信する前に貼った・ドロップした画像やファイル（本人だけが見られる一時置き場）。
-- 送信したとき、内容に残っているものを問い合わせの添付（attachment。id はそのまま）に移す。使われなかったものは1日たったら消す
create table pending_upload (
  id           uuid primary key,
  user_id      uuid not null references app_user(id) on delete cascade,
  project_id   uuid not null references project(id) on delete cascade,
  s3_key       text not null,
  file_name    text not null,
  size         bigint not null,
  content_type text not null,
  sha256       text not null,
  created_at   timestamptz not null default now()
);
create index pending_upload_user_idx on pending_upload (user_id, created_at);
