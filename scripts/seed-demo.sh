#!/usr/bin/env bash
# 開発用: デモのプロジェクト（DEMO）とタスクを作る（DEV_LOGIN=true の環境）。使い方: scripts/seed-demo.sh [BASE_URL]
set -euo pipefail
B=${1:-http://localhost:8090}
C=$(mktemp)
py() { python3 -c "import json,sys; d=json.load(sys.stdin); print($1)"; }
req() { if [ -n "${3:-}" ]; then curl -sS -b $C -c $C -X "$1" -H 'Content-Type: application/json' -d "$3" "$B$2"; else curl -sS -b $C -c $C -X "$1" "$B$2"; fi; }
curl -sS -c $C -X POST -H 'Content-Type: application/json' -d '{"name":"佐藤 花子","email":"sato@example.com"}' $B/auth/dev-login >/dev/null
SATO=$(req GET /api/me | py "d['id']")
req POST /auth/dev-login '{"name":"山田 太郎","email":"yamada@example.com","admin":true}' >/dev/null
P=$(req POST /api/projects '{"key":"DEMO","name":"デモ","description":"skea のデモ用プロジェクト"}' | py "d['id']")
req PUT /api/projects/$P/members "{\"userId\":\"$SATO\",\"role\":\"edit\"}"
META=$(req GET /api/projects/$P/meta)
st() { echo "$META" | py "[s['id'] for s in d['statuses'] if s['category']=='$1'][0]"; }
TODO=$(st todo); PROG=$(st in_progress); WAIT=$(st waiting); DONE=$(st done)
S1=$(req POST /api/projects/$P/sprints '{"name":"スプリント 1","goal":"ログイン機能を仕上げる"}' | py "d['id']")
req POST /api/projects/$P/sprints/$S1/start >/dev/null
req POST /api/projects/$P/sprints '{"name":"スプリント 2"}' >/dev/null
BUG=$(req POST /api/projects/$P/labels '{"name":"バグ","color":"#dc2626"}' | py "d['id']")
UI=$(req POST /api/projects/$P/labels '{"name":"UI","color":"#7c3aed"}' | py "d['id']")
req POST /api/projects/$P/fields '{"name":"顧客","kind":"select","options":["A社","B社","社内"],"required":false}' >/dev/null
ME=$(req GET /api/me | py "d['id']")
mk() { req POST /api/projects/$P/tasks "$1" | py "d['id']"; }
T1=$(mk "{\"title\":\"ログイン画面を作る\",\"sprintId\":\"$S1\",\"statusId\":\"$PROG\",\"assigneeId\":\"$ME\",\"labelIds\":[\"$UI\"],\"startDate\":\"2026-11-10\",\"dueDate\":\"2026-11-20\"}")
mk "{\"title\":\"パスワードの再設定\",\"sprintId\":\"$S1\",\"statusId\":\"$TODO\",\"assigneeId\":\"$SATO\",\"parentId\":\"$T1\",\"startDate\":\"2026-11-16\",\"dueDate\":\"2026-11-19\"}" >/dev/null
T3=$(mk "{\"title\":\"ログイン後に500エラーになる\",\"sprintId\":\"$S1\",\"statusId\":\"$WAIT\",\"assigneeId\":\"$SATO\",\"labelIds\":[\"$BUG\"],\"dueDate\":\"2026-11-12\"}")
mk "{\"title\":\"OIDC の設定画面\",\"sprintId\":\"$S1\",\"statusId\":\"$DONE\",\"assigneeId\":\"$ME\",\"startDate\":\"2026-11-03\",\"dueDate\":\"2026-11-07\"}" >/dev/null
mk "{\"title\":\"デザインの確認\",\"sprintId\":\"$S1\",\"statusId\":\"$TODO\",\"labelIds\":[\"$UI\"],\"startDate\":\"2026-11-20\",\"dueDate\":\"2026-11-27\"}" >/dev/null
mk '{"title":"ドキュメントを書く","description":"# 目的\n運用手順をまとめる"}' >/dev/null
mk '{"title":"監視の設定","startDate":"2026-11-24","dueDate":"2026-12-04"}' >/dev/null
req POST /api/tasks/$T3/dependencies '{"predecessorKey":"DEMO-1"}' || true
req POST /api/tasks/$T1/comments "{\"body\":{\"type\":\"doc\",\"content\":[{\"type\":\"paragraph\",\"content\":[{\"type\":\"mention\",\"attrs\":{\"id\":\"$SATO\",\"label\":\"佐藤 花子\"}},{\"type\":\"text\",\"text\":\" デザインを確認してください。\"}]}]}}" >/dev/null
req POST /api/tasks/$T1/worklogs '{"workDate":"2026-11-13","minutes":120,"note":"画面の作成"}' >/dev/null
echo "DEMO を作りました: $B"
