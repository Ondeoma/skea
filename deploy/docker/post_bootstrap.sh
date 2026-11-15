#!/bin/sh
# クラスタ初回作成時にプライマリで1度だけ実行される: アプリ用ロール・DB・PGroonga拡張を作る
set -e
psql -h /tmp -U postgres -v ON_ERROR_STOP=1 <<SQL
create role skea login password '${SKEA_DB_PASSWORD:-skea}';
create database skea owner skea;
\c skea
create extension if not exists pgroonga;
SQL
