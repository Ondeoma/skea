#!/usr/bin/env bash
# 開発環境（DEV_LOGIN=true）で API を一通り呼ぶ確認用スクリプト。使い方: scripts/smoke.sh [BASE_URL]
set -euo pipefail
B=${1:-http://localhost:8090}
J=$(mktemp -d)
C="$J/cookie"
req() { # method path [json]
  local m=$1 p=$2 d=${3:-}
  if [ -n "$d" ]; then curl -sS -b "$C" -c "$C" -X "$m" -H 'Content-Type: application/json' -d "$d" "$B$p"
  else curl -sS -b "$C" -c "$C" -X "$m" "$B$p"; fi
}
jqv() { python3 -c "import json,sys; d=json.load(sys.stdin); print(eval(sys.argv[1]))" "$1"; }
echo "== login";        req POST /auth/dev-login '{"name":"山田 太郎","email":"yamada@example.com","admin":true}' >/dev/null
echo "== me";           req GET /api/me | jqv "d['name']"
KEY=SM$(date +%H%M%S | cut -c1-6)
echo "== project $KEY"; P=$(req POST /api/projects "{\"key\":\"$KEY\",\"name\":\"スモーク\"}" | jqv "d['id']"); echo $P
META=$(req GET /api/projects/$P/meta)
ST_DONE=$(echo "$META" | jqv "[s['id'] for s in d['statuses'] if s['category']=='done'][0]")
echo "== sprint";       S=$(req POST /api/projects/$P/sprints '{"name":"スプリント1"}' | jqv "d['id']"); req POST /api/projects/$P/sprints/$S/start >/dev/null
echo "== label";        L=$(req POST /api/projects/$P/labels '{"name":"バグ"}' | jqv "d['id']")
echo "== field";        F=$(req POST /api/projects/$P/fields '{"name":"顧客","kind":"select","options":["A社","B社"],"required":false}' | jqv "d['id']")
echo "== task";         T=$(req POST /api/projects/$P/tasks "{\"title\":\"ログイン画面のバグ\",\"sprintId\":\"$S\",\"labelIds\":[\"$L\"],\"fields\":{\"$F\":\"A社\"},\"dueDate\":\"2026-12-01\"}")
TID=$(echo "$T" | jqv "d['id']"); TKEY=$(echo "$T" | jqv "d['key']"); echo $TKEY
echo "== subtask";      req POST /api/projects/$P/tasks "{\"title\":\"でざいん確認\",\"parentId\":\"$TID\"}" | jqv "d['key']"
echo "== patch";        req PATCH /api/tasks/$TID "{\"statusId\":\"$ST_DONE\",\"description\":\"# 手順\\n- 開く\\n- **押す**\"}" | jqv "d['resolvedAt'] is not None"
echo "== comment";      req POST /api/tasks/$TID/comments '{"body":"確認しました `code`"}' | jqv "d['html']"
echo "== worklog";      req POST /api/tasks/$TID/worklogs '{"workDate":"2026-11-15","minutes":90,"note":"調査"}' | jqv "d['minutes']"
echo "== detail";       req GET /api/tasks/$TKEY | jqv "(d['task']['spentMinutes'], d['descriptionHtml'])"
echo "== board";        req GET /api/projects/$P/board | jqv "[t['key'] for t in d['tasks']]"
echo "== search exact"; req GET "/api/search?q=%E3%83%AD%E3%82%B0%E3%82%A4%E3%83%B3%20project:$KEY" | jqv "([(h['task']['key'],h['fuzzy']) for h in d['hits']], d['error'])"
echo "== search kana";  req GET "/api/search?q=%E3%82%8D%E3%81%90%E3%81%84%E3%82%93%20project:$KEY" | jqv "[(h['task']['key'],h['fuzzy']) for h in d['hits']]"
echo "== search fuzzy"; req GET "/api/search?q=%E3%83%87%E3%82%B6%E3%82%A4%E3%82%BD%20project:$KEY" | jqv "[(h['task']['key'],h['fuzzy']) for h in d['hits']]"
echo "== search filter";req GET "/api/search?q=project:$KEY%20label:%E3%83%90%E3%82%B0%20field:%E9%A1%A7%E5%AE%A2=A%E7%A4%BE%20is:done" | jqv "([h['task']['key'] for h in d['hits']], d['error'])"
echo "== changes";      req GET "/api/projects/$P/changes?since=0" | jqv "[e['kind'] for e in d['events']]"
echo "== gantt";        req GET /api/projects/$P/gantt?done=true | jqv "len(d['tasks'])"
echo "== worklogs";     req GET "/api/worklogs?from=2026-11-01&to=2026-11-30" | jqv "len(d)"
echo "== api key";      K=$(req POST /api/me/api-keys '{"name":"mcp","scopes":["read","write"]}' | jqv "d['key']")
echo "== mcp tools";    curl -sS -H "Authorization: Bearer $K" -H 'Content-Type: application/json' -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' $B/mcp | jqv "[t['name'] for t in d['result']['tools']]"
echo "== mcp create";   curl -sS -H "Authorization: Bearer $K" -H 'Content-Type: application/json' -d "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/call\",\"params\":{\"name\":\"create_task\",\"arguments\":{\"projectKey\":\"$KEY\",\"title\":\"MCPから\",\"assignee\":\"me\",\"labels\":[\"バグ\"],\"sprint\":\"active\"}}}" $B/mcp | jqv "d['result']['content'][0]['text']"
echo "== mcp board";    curl -sS -H "Authorization: Bearer $K" -H 'Content-Type: application/json' -d "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"get_board\",\"arguments\":{\"projectKey\":\"$KEY\"}}}" $B/mcp | jqv "d['result']['content'][0]['text']"
echo "== mcp call_api"; curl -sS -H "Authorization: Bearer $K" -H 'Content-Type: application/json' -d "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",\"params\":{\"name\":\"call_api\",\"arguments\":{\"method\":\"GET\",\"path\":\"/api/projects/$P/sprints\"}}}" $B/mcp | jqv "d['result']['isError']"
echo "== automation"
req POST /api/projects/$P/automations '{"name":"作成時にコメント","enabled":true,"trigger":{"type":"task_created"},"actions":[{"type":"if","condition":{"field":"task.title","op":"contains","value":"自動"},"then":[{"type":"comment","text":"自動コメント: {{task.key}} 期日 {{task.dueDate | default:なし}}"},{"type":"add_label","label":"自動"},{"type":"set_field","field":"dueDate","value":"{{today+7}}"}],"else":[{"type":"log","message":"対象外"}]}]}' | jqv "d['name']"
AT=$(req POST /api/projects/$P/tasks '{"title":"自動化のテスト"}' | jqv "d['key']")
sleep 8
req GET /api/tasks/$AT | jqv "(d['task']['dueDate'], d['task']['labelIds'] != [])"
req GET /api/tasks/$AT/comments | jqv "[c['html'] for c in d]"
req GET /api/projects/$P/automation-runs | jqv "[(r['status'], [l['message'] for l in r['log']]) for r in d]"
echo "== mention notification"
U2=$(curl -sS -c "$J/c2" -X POST -H 'Content-Type: application/json' -d '{"name":"佐藤 花子","email":"sato@example.com"}' $B/auth/dev-login >/dev/null; curl -sS -b "$J/c2" $B/api/me | jqv "d['id']")
req PUT /api/projects/$P/members "{\"userId\":\"$U2\",\"role\":\"edit\"}"
req POST /api/tasks/$TID/comments "{\"body\":{\"type\":\"doc\",\"content\":[{\"type\":\"paragraph\",\"content\":[{\"type\":\"mention\",\"attrs\":{\"id\":\"$U2\",\"label\":\"佐藤 花子\"}},{\"type\":\"text\",\"text\":\" 見てください\"}]}]}}" >/dev/null
curl -sS -b "$J/c2" "$B/api/notifications?kinds=mention" | jqv "[n['title'] for n in d['items']]"
echo "== scm webhook (GitHub)"
R=$(req POST /api/projects/$P/repos '{"provider":"github","name":"example/app","webUrl":"https://github.com/example/app"}')
RID=$(echo "$R" | jqv "d['id']"); SEC=$(echo "$R" | jqv "d['secret']")
BODY="{\"ref\":\"refs/heads/main\",\"commits\":[{\"id\":\"abc123def456\",\"message\":\"$TKEY ログインを直す\",\"url\":\"https://github.com/example/app/commit/abc123\",\"timestamp\":\"2026-11-15T10:00:00+09:00\",\"author\":{\"name\":\"dev\"}}]}"
SIG=$(printf '%s' "$BODY" | openssl dgst -sha256 -hmac "$SEC" | sed 's/^.* //')
curl -sS -X POST -H 'X-GitHub-Event: push' -H "X-Hub-Signature-256: sha256=$SIG" -H 'Content-Type: application/json' --data-binary "$BODY" $B/hooks/scm/$RID; echo
curl -sS -o /dev/null -w 'bad signature -> %{http_code}\n' -X POST -H 'X-GitHub-Event: push' -H "X-Hub-Signature-256: sha256=00" --data-binary "$BODY" $B/hooks/scm/$RID
req GET /api/tasks/$TKEY | jqv "[c['sha'] for c in d['commits']]"
echo "== notifications";req GET /api/notifications/unread
echo; echo OK
