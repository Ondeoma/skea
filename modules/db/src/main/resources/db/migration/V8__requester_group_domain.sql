-- "ユーザ"のグループのメールのドメイン（例: a-sha.example.com）。同じドメインのメールアドレスの"ユーザ"は、
-- 自分の問い合わせをこのグループに共有できる。空なら親のグループのドメインを引き継ぐ
alter table requester_group add column domain text;
