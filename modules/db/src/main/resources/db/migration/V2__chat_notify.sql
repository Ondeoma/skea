-- 通知の種類ごとに、チャット（stui）にも送るか（行がなければ送る）
alter table notify_pref add column chat boolean not null default true;
