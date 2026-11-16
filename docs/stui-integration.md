# stui 連携の取り決め（skea から stui へ通知を送る向き）

skea の通知（担当・メンション・コメント・期日など）を、連携したユーザーには stui（チャット）のボット「skea」からの **DM** でも送る。
仕組みは [skiwi 連携](skiwi-integration.md)・[skawau 連携](skawau-integration.md) と同じ（OAuth 2.0 の認可コード + PKCE）。
stui 側の仕様は stui の `docs/integrations.md` の「1. 兄弟 → stui」。

- 各ユーザーは skea の個人設定で「stui と連携する」を押し、stui の画面で許可する（スコープ `read write`）。
- 通知の種類ごとに、stui にも送るかを個人設定で選べる（`notify_pref.chat`。既定は送る）。
- 送信は裏で行い、失敗しても skea の操作は止めない。stui が `401` を返したら連携を外す（トークンが失効している）。

## stui 側に必要なもの

`OAUTH_CLIENTS` に skea を登録する。

```json
[{"id": "skea", "secret": "（16文字以上。skea の STUI_CLIENT_SECRET と同じ）",
  "redirectUri": "{skea の URL}/api/integrations/stui/callback", "name": "skea", "scopes": ["read", "write"]}]
```

開発環境: `client_id=skea`、`secret=skea-dev-secret-0123456789`、`redirectUri=http://localhost:8090/api/integrations/stui/callback`。

使う API:

- `GET /oauth/authorize`・`POST /oauth/token`・`POST /oauth/revoke`（skiwi 連携と同じ形）
- `POST /api/apps/notify`（`Authorization: Bearer stui_…`）: `{"title": "DEMO-12 の担当になりました", "text": "操作した人: 本文", "url": "{skea の URL}/tasks/DEMO-12"}`。
  `channel` は付けない（トークンの持ち主への DM になる）。

## skea 側（このリポジトリ）

- **環境変数**: `STUI_URL`（利用者のブラウザから見た stui の URL）、`STUI_CLIENT_ID`、`STUI_CLIENT_SECRET`、
  `STUI_INTERNAL_URL`（サーバーから呼ぶときの URL。省略時は `STUI_URL`）。開発環境では `STUI_URL=http://localhost:5178`。
- **エンドポイント**: skiwi・skawau と共通（`LinkServiceRoutes`）。`/api/integrations/stui/{connect,callback,connection}`。
- **送信**: `ChatNotifier`（`NotifyService.send` から、画面内の通知を作ったあとに呼ぶ）。

## stui から skea を見る向き

stui は skea の `POST /api/tasks/lookup` で、貼られたタスクの URL にタイトルを出す。skea の `OAUTH_CLIENTS` に stui を登録する
（開発環境: `id=stui`、`secret=stui-dev-secret-0123456789`、`redirectUri=http://localhost:5178/api/integrations/skea/callback`）。
