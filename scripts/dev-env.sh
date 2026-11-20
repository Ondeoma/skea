# 開発用の環境変数（source scripts/dev-env.sh）。docker-compose.dev.yml の DB・ScalaS3・Mailpit を使う
export DATABASE_URL=jdbc:postgresql://localhost:54339/skea DATABASE_USER=skea DATABASE_PASSWORD=skea
export PUBLIC_URL=http://localhost:8090 HTTP_PORT=8090
export DEV_LOGIN=true
# 回数の制限（IP ごとの1分あたり）。手元ではすべての要求が 127.0.0.1 から来るので、開発中は緩める
export RATE_LIMIT_AUTH=${RATE_LIMIT_AUTH:-300}
export S3_ENDPOINT=http://localhost:3910 S3_BUCKET=skea S3_ACCESS_KEY=skea S3_SECRET_KEY=skeaskeaskeaskea
export SMTP_HOST=localhost SMTP_PORT=1026 SMTP_TLS=none MAIL_FROM=skea@example.com
export WEBHOOK_ALLOW_HTTP=true
# skiwi・stui・skakapo（エージェント. 読み書き）との連携（開発用。本番では client の secret をランダムな値にする）
export OAUTH_CLIENTS='[{"id":"skiwi","secret":"skiwi-dev-secret-0123456789","redirectUri":"http://localhost:5173/api/integrations/skea/callback","name":"skiwi"},{"id":"stui","secret":"stui-dev-secret-0123456789","redirectUri":"http://localhost:5178/api/integrations/skea/callback","name":"stui"},{"id":"skakapo","secret":"skakapo-dev-secret-0123456789","redirectUri":"http://localhost:5199/api/connections/callback","name":"skakapo","scopes":["read","write"]}]'
# skea から skiwi を見る向きの連携（skiwi に client_id=skea として登録する。開発用の値）
export SKIWI_URL=http://localhost:5173 SKIWI_CLIENT_ID=skea SKIWI_CLIENT_SECRET=skea-dev-secret-0123456789
# skea から skawau を見る向きの連携（skawau に client_id=skea として登録する。URL は skawau の画面の開発サーバー）
export SKAWAU_URL=http://localhost:5176 SKAWAU_CLIENT_ID=skea SKAWAU_CLIENT_SECRET=skea-dev-secret-0123456789
# skea から stui（チャット）へ通知を送る連携（stui に client_id=skea として登録する。URL は stui の画面の開発サーバー）
export STUI_URL=http://localhost:5178 STUI_CLIENT_ID=skea STUI_CLIENT_SECRET=skea-dev-secret-0123456789
