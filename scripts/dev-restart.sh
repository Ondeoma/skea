#!/usr/bin/env bash
# 開発用: 8090 番のサーバーを止めて、起動し直す（ログは $LOG、既定 /tmp/skea-dev.log）。起動を待ってから戻る
cd "$(dirname "$0")/.."
LOG=${LOG:-/tmp/skea-dev.log}
pid=$(lsof -tiTCP:${HTTP_PORT:-8090} -sTCP:LISTEN || true); [ -n "$pid" ] && kill $pid && sleep 2
. scripts/dev-env.sh
nohup sbt -batch -no-colors app/run > "$LOG" 2>&1 &
for i in $(seq 1 180); do
  grep -qE 'listening on' "$LOG" && { echo "started"; exit 0; }
  grep -qE 'nonzero exit|\[error\]' "$LOG" && { tail -30 "$LOG"; exit 1; }
  sleep 1
done
echo "timeout"; exit 1
