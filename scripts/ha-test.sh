#!/usr/bin/env bash
# 3ノード試験環境（scripts/ha-up.sh）の障害試験。
#  1. アプリ1台を止めても、HAProxy 経由で作成・読み出しができる
#  2. DB のプライマリを止めても、フェイルオーバー後に作成でき、止める前のデータも残っている
set -euo pipefail
cd "$(dirname "$0")/.."
B=${BASE:-http://localhost:8091}
DC="docker compose -f docker-compose.ha.yml"
C=$(mktemp)
py() { python3 -c "import json,sys; d=json.load(sys.stdin); print($1)"; }
login() { curl -sf -c $C -X POST -H 'Content-Type: application/json' -d '{"name":"HA 試験","admin":true}' $B/auth/dev-login >/dev/null; }
create() { curl -sf -b $C -X POST -H 'Content-Type: application/json' -d "{\"title\":\"$1\"}" $B/api/projects/$P/tasks | py "d['key']"; }
retry() { for i in $(seq 1 60); do if out=$("$@" 2>/dev/null); then echo "$out"; return 0; fi; sleep 2; done; return 1; }

login
KEY=HA$(date +%H%M%S | cut -c3-6)
P=$(curl -sf -b $C -X POST -H 'Content-Type: application/json' -d "{\"key\":\"$KEY\",\"name\":\"HA\"}" $B/api/projects | py "d['id']")
echo "project $KEY"
K0=$(create "停止前")

echo "== アプリ app1 を停止"
$DC stop app1 >/dev/null
sleep 5
for i in 1 2 3 4 5 6; do create "app1 停止中 $i" >/dev/null; done
echo "OK: app1 停止中も6件作成できた"
$DC start app1 >/dev/null

echo "== DB プライマリを停止"
LEADER=$($DC exec -T pg1 patronictl -c /etc/patroni/patroni.yml list -f json | py "[m['Member'] for m in d if m['Role']=='Leader'][0]")
echo "leader: $LEADER"
$DC stop "$LEADER" >/dev/null
T0=$(date +%s)
K1=$(retry create "フェイルオーバー後")
echo "OK: $(( $(date +%s) - T0 )) 秒後に作成できた ($K1)"
retry curl -sf -b $C $B/api/tasks/$K0 | py "'OK: 停止前のタスク ' + d['task']['key'] + ' が残っている'"
$DC start "$LEADER" >/dev/null
sleep 20
$DC exec -T pg1 patronictl -c /etc/patroni/patroni.yml list || true
N=$(curl -sf -b $C "$B/api/search?q=project:$KEY" | py "d['total']")
echo "タスク数: ${N}（期待 8）"
[ "$N" = "8" ] && echo "ALL OK"
