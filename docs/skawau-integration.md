# skawau 連携の取り決め（skea から skawau を見る向き）

skea の課題の説明・リンク・コメントに貼られた skawau のファイル・フォルダの URL に、
**ログイン中のユーザー自身の skawau での権限** でファイル名を表示する。仕組みは [skiwi 連携](skiwi-integration.md) と同じ。

- 各ユーザーは skea の個人設定で「skawau と連携する」を押し、skawau の画面で許可する（OAuth 2.0 の認可コード + PKCE）。
- skawau は、そのユーザーの **読み取り専用** のトークンを発行する。skea はそれを暗号化して保存し、サーバーから skawau の API を呼ぶ。
- 連携していないユーザーには、URL をそのまま表示する（ファイル名は出さない）。

## skawau 側に必要なもの

### 1. クライアントの登録

| 項目 | 例 |
|---|---|
| `client_id` | `skea` |
| `client_secret` | ランダムな文字列（skea の環境変数 `SKAWAU_CLIENT_SECRET` に同じ値を入れる） |
| `redirect_uri` | `{skea の URL}/api/integrations/skawau/callback`（完全一致で照合する） |
| 表示名 | `skea`（許可画面に出す） |

開発環境: `client_id=skea`、`client_secret=skea-dev-secret-0123456789`、`redirect_uri=http://localhost:8090/api/integrations/skawau/callback`。

### 2. `GET /oauth/authorize`・`POST /oauth/token`・`POST /oauth/revoke`

skiwi 連携の取り決め（[skiwi-integration.md](skiwi-integration.md) の 2.）と同じ。許可画面の文言は
「skea が、あなたの権限で skawau のファイル名を読み取ります」。トークンの名前は「skea 連携」。

### 3. `POST /api/files/lookup`（ファイル・フォルダの名前をまとめて読む）

`Authorization: Bearer <access_token>`（read のトークンで POST を受け付ける）。

```json
{ "ids": ["5f0c…-…", "…"] }
```

`ids` はファイルまたはフォルダの ID（UUID）で最大100件。トークンの持ち主が見られるもの（自分のもの・共有されたもの）だけを返す
（ごみ箱の中・存在しない・権限のないものは区別せず返さない）。

```json
{ "files": [
  { "id": "5f0c…", "name": "設計書.docx", "folder": false }
] }
```

トークンが失効しているときは `401` を返す（skea は連携が切れたと扱う）。

### 4. URL の形

`{skawau の URL}/view/{ID}`・`/office/{ID}`・`/files/{ID}`（後ろに `?…`・`#…`・`/…` が付いてもよい）。skea は ID を取り出して問い合わせる。
公開リンク（`/s/{トークン}`）は対象にしない。

## skea 側（このリポジトリ）

- **環境変数**: `SKAWAU_URL`（利用者が貼る画面の URL）、`SKAWAU_CLIENT_ID`、`SKAWAU_CLIENT_SECRET`。3つとも設定したときだけ使える。
  開発環境では `SKAWAU_URL=http://localhost:5176`（skawau の画面の開発サーバー。`/oauth`・`/api` を skawau のサーバーへ中継する）。
- **エンドポイント**: skiwi と共通（`LinkServiceRoutes`）。`/api/integrations/skawau/{connect,callback,connection,titles}`。
- **表示**: `modules/web/src/main/scala/skea/web/RichView.scala`（LinkTitles）。ファイル名の前に拡張子に応じたアイコン（📁 フォルダ、📝 文書、📊 表、📽️ スライド、📕 PDF、🖼️ 画像 など）を付ける。
  リンクの文字が URL のままならファイル名に置き換え、文字を書いてあるリンクはそのままにして、ファイル名を補足として出す。編集中のエディタの中は書き換えない。
