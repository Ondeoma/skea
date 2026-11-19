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

## プロジェクトの通知を stui のチャンネルに送る

プロジェクトの設定の「Webhook」で「💬 stui に通知」を選ぶと、URL や秘密の値を用意しなくても、イベントを stui のチャンネルに文で投稿できる。

- 送信先の種類が `stui` の Webhook（`webhook.kind = 'stui'`）。イベントの選び方・送信待ち・再送・送信履歴は、URL の Webhook と同じ。
- 保存した人（`owner_id`）の stui との連携（上の個人の連携と同じトークン）で、その人が参加しているチャンネル（`channel_id`）に、
  ボット「skea」として投稿する（`POST /api/apps/notify` に `channel` を付ける）。保存するには、保存する人が stui と連携している必要がある。
- チャンネルの選択肢: `GET /api/integrations/stui/channels`（skea が stui の `GET /api/channels` を、その人のトークンで読む。公開・非公開で、アーカイブしていないもの）。
  連携していなければ `409`。
- 文はイベントごとに組み立てる（`ChatMessage`）。見出しはタスクのキーとタイトル（タスクへのリンク）、本文は操作した人と変わったこと。
- stui が `401` を返したら、その人の連携を外し、送信を「失敗」にする（その人が連携し直して、保存し直す）。`403`（チャンネルを抜けた）・`404` も失敗にする。

### チャンネルの管理者なら、受信 Webhook を作って送れる（`webhook.kind = 'stui_hook'`）

- チャンネルの一覧の `admin`（stui の `role` が `admin`）が true のチャンネルを選ぶと、画面に「stui に受信 Webhook を作って送る」（既定）が出る。
- 保存すると、保存した人の連携で stui の `POST /api/hooks/incoming`（`{"channelId", "name": "skea: {キー} {名前}", "username": "skea"}`）を呼び、
  返った ID を `stui_hook_id`、URL を `url` に保存する。URL は応答の `/hooks/{トークン}` だけを取り、`STUI_INTERNAL_URL`（省略時は `STUI_URL`）に付ける。
- 送るときはトークンを使わず、その URL に `{"text": "**[見出し](リンク)**\n\n本文", "username": "skea"}` を POST する。
  作った人が連携を外したりチャンネルを抜けたりしても届く。`404`（受信 Webhook が消された）・`403`（アーカイブ）は失敗にする。
- 名前・イベント・有効だけを変えるときは、同じ受信 Webhook を使い続ける（連携していない管理者でも保存できる）。
  チャンネルや送り方を変えたとき・削除したときは、作った人の連携で stui の受信 Webhook を消す（消せなくても続ける）。

## stui から skea を見る向き

stui は skea の `POST /api/tasks/lookup` で、貼られたタスクの URL にタイトルを出す。skea の `OAUTH_CLIENTS` に stui を登録する
（開発環境: `id=stui`、`secret=stui-dev-secret-0123456789`、`redirectUri=http://localhost:5178/api/integrations/skea/callback`）。
