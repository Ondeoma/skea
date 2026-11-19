-- Webhook の送信先に stui のチャンネルを足す。
--  kind = 'stui':      設定した人（owner_id）が stui と OAuth で連携したトークンで、その人が参加しているチャンネル（channel_id）に
--                      ボット「skea」として投稿する（url と secret は使わない）
--  kind = 'stui_hook': チャンネルの管理者が、連携を使って stui にそのチャンネルの受信 Webhook（stui_hook_id）を作り、その URL（url）に送る。
--                      作った人が連携を外したりいなくなったりしても届く（owner_id は受信 Webhook を消すときに使う）
alter table webhook
  add column kind         text not null default 'url' check (kind in ('url', 'stui', 'stui_hook')),
  add column owner_id     uuid references app_user(id) on delete set null,
  add column channel_id   text,
  add column channel_name text,
  add column stui_hook_id text;
