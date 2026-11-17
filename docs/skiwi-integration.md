# skiwi 連携の取り決め（skea から skiwi を見る向き）

skea の課題の説明・リンク・コメントに貼られた skiwi のページの URL（`{skiwi}/w/{ワークスペースID}/p/{ページID}`）に、
**ログイン中のユーザー自身の skiwi での権限** でページのタイトルを表示する。
[skea → skiwi の向き](https://github.com/Ondeoma/skiwi/blob/main/docs/skea-integration.md)（skiwi に skea のタスクを出す）をちょうど逆にしたもの。

- 各ユーザーは skea の個人設定で「skiwi と連携」を押し、skiwi の画面で許可する（OAuth 2.0 の認可コード + PKCE）。
- skiwi は、そのユーザーの **読み取り専用** のトークンを発行する。skea はそれを暗号化して保存し、サーバーから skiwi の API を呼ぶ。
- 連携していないユーザーには、URL をそのまま表示する（タイトルは出さない）。

## skiwi 側に必要なもの

### 1. クライアントの登録

skiwi の管理者が、skea をクライアントとして登録する（環境変数でも管理画面でもよい）。

| 項目 | 例 |
|---|---|
| `client_id` | `skea` |
| `client_secret` | ランダムな文字列（skea の環境変数 `SKIWI_CLIENT_SECRET` に同じ値を入れる） |
| `redirect_uri` | `{skea の URL}/api/integrations/skiwi/callback`（完全一致で照合する） |
| 表示名 | `skea`（許可画面に出す） |

開発環境: `client_id=skea`、`client_secret=skea-dev-secret-0123456789`、`redirect_uri=http://localhost:8090/api/integrations/skiwi/callback`。

### 2. `GET /oauth/authorize`・`POST /oauth/token`・`POST /oauth/revoke`

skea 側に実装したもの（skea の `/oauth/*`）と同じ形にする。

- authorize: `response_type=code`、`client_id`、`redirect_uri`、`scope=read`、`state`、`code_challenge`、`code_challenge_method=S256`（S256 以外は拒否）。
  - 未ログインならログインしてから戻る。「skea が、あなたの権限で skiwi のページのタイトルを読み取ります」と許可・拒否を出す。
  - 許可: `302 {redirect_uri}?code=…&state=…`。拒否: `302 {redirect_uri}?error=access_denied&state=…`。
  - `client_id`・`redirect_uri` が不正なときは、リダイレクトせず画面でエラーを出す。
  - 認可コードは60秒・一度だけ。client・redirect_uri・ユーザー・code_challenge に結び付ける。
- token: `application/x-www-form-urlencoded`。クライアント認証は HTTP Basic。`grant_type=authorization_code`、`code`、`redirect_uri`、`code_verifier`。
  - 成功（`Cache-Control: no-store`）: `{"access_token":"…","token_type":"Bearer","scope":"read"}`。
  - トークンはそのユーザーの読み取り専用の API トークン（名前「skea 連携」、期限なし、skiwi の API トークンの画面から失効できる）でよい。
  - 同じユーザーが許可し直したら、以前のトークンを失効させる。
  - エラーは RFC 6749 の形（`invalid_request`・`invalid_client`(401)・`invalid_grant`・`unsupported_grant_type`）。
- revoke（RFC 7009）: `token=…` を Basic 認証付きで受け取り、そのクライアントが発行したトークンを失効させる。なくても `200`。

### 3. `POST /api/pages/lookup`（ページをまとめて読む）

`Authorization: Bearer <access_token>`。

```json
{ "ids": ["5f0c…-…", "…"] }
```

`ids` はページの ID（UUID）で最大100件。トークンの持ち主が見られるページだけを返す（削除済み・存在しない・権限のないものは区別せず返さない）。

```json
{ "pages": [
  { "id": "5f0c…", "workspaceId": "a1b2…", "title": "設計メモ", "icon": "📘" }
] }
```

`icon` はなければ `null`。トークンが失効しているときは `401` を返す（skea は連携が切れたと表示する）。

### 4. ページの URL

`{skiwi の URL}/w/{ワークスペースID}/p/{ページID}`（後ろに `#…` や `/…` が付いてもよい）。skea はページID を取り出して問い合わせる。

## skea 側（このリポジトリ）

- **環境変数**: `SKIWI_URL`、`SKIWI_CLIENT_ID`、`SKIWI_CLIENT_SECRET`。3つとも設定したときだけ連携を使える。
- **エンドポイント**:
  - `GET /api/integrations/links`: 設定されたサービス（skiwi・skawau）の URL と、自分が連携済みかどうか
  - `GET /api/integrations/skiwi/connect` → skiwi の `/oauth/authorize` へリダイレクトする。`state` と `code_verifier` は DB に保存する（10分）
  - `GET /api/integrations/skiwi/callback`: コードをトークンに替え、暗号化して保存して、個人設定へ戻る（`?integration=skiwi&result=connected`）
  - `DELETE /api/integrations/skiwi/connection`: skiwi の `/oauth/revoke` を呼んでから削除する
  - `POST /api/integrations/skiwi/titles` `{ids}`: 本人のトークンで skiwi の `/api/pages/lookup` を呼ぶ。60秒キャッシュする
- **表示**: 説明・コメント・リンクの一覧の、skiwi のページへのリンクを探してタイトルを問い合わせる。
  リンクの文字が URL のままなら「📘 タイトル」に置き換え、文字を書いてあるリンクはそのままにして、タイトルを補足として出す。
  編集中のエディタの中は書き換えない。

skawau のファイル名を出す連携も同じ仕組み（`LinkServiceRoutes`・web の `RichView.scala` の `LinkTitles`）で動く。[docs/skawau-integration.md](skawau-integration.md) を参照。
