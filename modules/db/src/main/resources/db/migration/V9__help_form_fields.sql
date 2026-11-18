-- ヘルプデスク: 窓口の「新しい問い合わせ」で"ユーザ"が入力する項目（カスタムフィールドから選ぶ）と、その入力のヒント
alter table custom_field add column help_form boolean not null default false;
alter table custom_field add column help_hint text not null default '';
