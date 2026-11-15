# skea

<p align="center"><img src="frontend/public/logo.svg" width="128" height="128" alt="skea のロゴ"></p>

skea は、チームのためのタスク管理システムです（プロジェクト・タスク・スプリント・カンバン・ガントチャート）。ライセンス: MIT

- **カンバン**（アクティブなスプリントのタスクをステータスの列に表示。ドラッグで移動し、落とす位置が空いて並び替わる様子が分かる。
  自分の担当で絞り込み、担当者・ラベルごとのスイムレーン、クリックで詳細をモーダル表示。タッチ端末では長押しでドラッグ）、**バックログ**、**一覧・検索**（担当者・ステータス・日付などの条件の欄）、
  **ガントチャート**（依存関係をバーの右端からドラッグでつなぐ・矢印のクリックで外す、クリティカルパスの強調、
  先行タスクを動かすと後続も一緒にずらす、マイルストーン、任意階層のサブタスク）
- **タスク**: タスクID（`SKEA-123`）、タイトル、説明（リッチテキスト）、期日・開始日、ラベル、スプリント、ステータス（作業前・進行中・待機中・完了。追加・変更可）、
  リンク（別タスク・URL。skiwi のページ・skawau のファイルは名前を表示）、担当者、報告者、ウォッチャー（ほかの人も追加できる）、作業時間、添付ファイル、
  カスタムフィールド（テキスト・複数行・チェックボックス・ラジオ・選択肢）
- **コメント**: WYSIWYG（Tiptap）、@メンション、`/` のメニュー、画像の貼り付け（大きさ・寄せ方向）、表（セルの背景色）、
  言語を選べるコードブロック（シンタックスハイライト）、コメントごとのアンカーリンク、入力中の下書きの自動保存（ユーザー＋タスクごと）、
  絵文字のリアクション（カスタム絵文字も登録できる）、人・グループ・役割（管理者・編集者・全員）へのメンション
- **テンプレート・繰り返しタスク**: よく作るタスクの初期値とサブタスクをテンプレートにし、毎日・毎週・毎月の決まった日に自動で作る
- **取り込み**: CSV や Jira の書き出し（CSV）から、ステータス・担当者・ラベル・親子関係を対応付けて取り込む
- **ユーザー**: アイコンの画像を登録できる（IdP の画像より優先）。画面の色は自動（OS に合わせる）・ライト・ダークを選べる
- **タスクの優先度と種類**: 優先度（最高〜最低）と種類（タスク・バグ・ストーリー・エピック・改善）。カード・一覧・検索（`priority:high type:bug`）で使える
- **一覧の表示の切り替え**: 表示する列の選択、ステータス・担当者・優先度などでのグループ化、一覧の中での直接の編集
- **自動同期**: 変更はサーバーから画面へ即座に知らせ（Server-Sent Events）、ほかの人の変更を通知して表示を更新（つながらないときは15秒ごとに確認）
- **通知**: 画面内（メンション・担当・コメントなどで絞り込み）、メール、Webhook
- **自動化**: イベント・定期・手動・受信 Webhook をきっかけに、条件分岐・Web API の呼び出しを含むアクションを実行
- **検索**: 日本語の全文検索（PGroonga）。かな・全角半角の揺れ、入力ミスを許す曖昧検索、`status:進行中 assignee:me due<+7d` のような絞り込み
- **作業時間の一覧**: 誰が日ごとにどのタスクを何時間したか（人×日、メンバーを選んでタスク×日、日ごとの明細）
- **連携**: GitHub / GitLab / Bitbucket / Forgejo（コミットメッセージのタスクIDで自動リンク）、skiwi（ページのタイトル）・skawau（ファイル名）
- **API・AI**: ユーザーごとのスコープ付き APIキー、REST API、MCP（AI から画面と同じ操作ができる）
- **認証・権限**: OIDC、システム管理者・一般、プロジェクトごとの権限（管理・編集・閲覧。ユーザー・IdP のグループ単位）
- **冗長化**: アプリはステートレスで複数ノードで動く。PostgreSQL は Patroni、添付は S3 互換ストレージ
- レスポンシブ対応、ダークモード対応

設計は [docs/design.md](docs/design.md)、構築・運用は [docs/operations.md](docs/operations.md) を参照してください。

## 構成

| モジュール | 内容 |
|---|---|
| `modules/shared` | バックエンドと画面（Scala.js）で共有する API の型（circe） |
| `modules/core` | ドメインの純粋なロジック（権限・並び順・タスクID・検索の絞り込み構文・リッチテキストの変換・自動化の条件とスマート値） |
| `modules/db` | doobie のストア、Flyway のマイグレーション、ジョブのリース、送信待ちの表の処理 |
| `modules/auth` | OIDC（Authorization Code + PKCE）、APIキー、暗号化 |
| `modules/storage` | S3 互換ストレージ（添付ファイル） |
| `modules/notify` | メール（SMTP） |
| `modules/integrations` | SCM（Webhook の解析・署名の検証・API） |
| `modules/api` | http4s のルート（REST・MCP・受信 Webhook）と、通知・自動化・Webhook 送信のサービス |
| `modules/app` | 起動・設定（環境変数）・画面の配信 |
| `modules/web` | 画面（[Scalactive](https://github.com/Ondeoma/scalactive) の `.st` 単一ファイルコンポーネント） |
| `frontend/` | Vite。Scala.js の出力と Tiptap（エディタ）をまとめてビルドする |
| `deploy/` | Docker（試験用の3ノード構成）と VM 構築用の設定のひな形 |

技術: Scala 3.3 LTS、Cats Effect 3、http4s 1.0（マイルストーン版）、doobie、Flyway、PostgreSQL 17 + PGroonga、Scala.js + Scalactive、Tiptap。

## 開発

必要なもの: JDK 21、sbt、Node.js、Docker。

```bash
docker compose -f docker-compose.dev.yml up -d --wait   # PostgreSQL+PGroonga (:54339)、ScalaS3 (:3910)、Mailpit (SMTP :1026、画面 :8026)
scripts/dev-restart.sh                                   # バックエンドを :8090 で起動（scripts/dev-env.sh の設定。DEV_LOGIN=true）
(cd frontend && npm install && npm run dev)              # 画面の開発サーバー http://localhost:5174（/api などは :8090 へ中継）
sbt ~web/fastLinkJS                                      # 画面の変更を再リンクし続ける（ブラウザを再読み込みすると反映）
```

- 開発用のログイン（`DEV_LOGIN=true`）では、名前を入れるだけでログインできます。本番では使わないでください。
- `scripts/seed-demo.sh` でデモ用のプロジェクト（DEMO）を作れます。`scripts/smoke.sh` で API を一通り確認できます。
- 送信されたメールは Mailpit（http://localhost:8026）で見られます。

テスト:

```bash
sbt test
```

**DB マイグレーション**（`modules/db/src/main/resources/db/migration`）: スキーマを変えるときは新しい番号の `V<n>__*.sql` を追加し、適用済みのファイルは書き換えないでください。

## 配布・起動

```bash
scripts/dist.sh                   # dist/lib/*.jar と dist/static（画面）を作る
java -cp 'dist/lib/*' skea.app.Main
```

主な環境変数（すべては `modules/app/src/main/scala/skea/app/Main.scala` の `Config`）:

| 変数 | 内容 |
|---|---|
| `DATABASE_URL` / `DATABASE_USER` / `DATABASE_PASSWORD` | PostgreSQL（PGroonga が必要）。冗長構成では3台を並べて `targetServerType=primary` |
| `PUBLIC_URL` | 公開 URL（`https://` なら Cookie に Secure が付く） |
| `OIDC_ISSUER` / `OIDC_CLIENT_ID` / `OIDC_CLIENT_SECRET` | OIDC。Redirect URI は `${PUBLIC_URL}/auth/callback` |
| `OIDC_GROUPS_CLAIM` | 所属グループのクレーム名（既定 `groups`） |
| `ADMIN_GROUPS` | システム管理者にするグループ（カンマ区切り）。画面からも管理者を付けられる |
| `APP_SECRET` | 秘密の値の暗号化（SCM のトークン・外部サービスの OAuth トークン）に使う16文字以上の値 |
| `STATIC_DIR` | ビルド済みの画面（`dist/static`） |
| `S3_ENDPOINT` / `S3_BUCKET` / `S3_ACCESS_KEY` / `S3_SECRET_KEY` | 添付ファイル（未設定なら添付は使えない） |
| `SMTP_HOST` / `SMTP_PORT` / `SMTP_TLS` / `SMTP_USER` / `SMTP_PASSWORD` / `MAIL_FROM` | メール（未設定ならメールは送らない） |
| `MAX_UPLOAD_MB` | 添付の上限（既定 100） |
| `TIMEZONE` | 期限の通知などの「今日」（既定 Asia/Tokyo） |
| `WEBHOOK_ALLOW_HTTP` | Webhook の送信先に http:// を許す（開発用） |
| `SKIWI_URL`・`SKIWI_CLIENT_ID`・`SKIWI_CLIENT_SECRET` | skiwi のページのタイトルを出す連携（skiwi に skea をクライアントとして登録した値。[docs/skiwi-integration.md](docs/skiwi-integration.md)） |
| `SKAWAU_URL`・`SKAWAU_CLIENT_ID`・`SKAWAU_CLIENT_SECRET` | skawau のファイル名を出す連携（skawau に skea をクライアントとして登録した値。[docs/skawau-integration.md](docs/skawau-integration.md)） |
| `OAUTH_CLIENTS` | 認可を求める外部のサービス（skiwi など）。`[{"id":"skiwi","secret":"…（16文字以上）","redirectUri":"https://…/callback","name":"skiwi"}]` |
| `NODE_ID` | ノード名（ログ・定期処理のリース） |
| `DEV_LOGIN` | 開発用のログインを有効にする（OIDC を設定しない場合のみ） |

## API・MCP

- 画面の「個人設定」で APIキー（`skea_` で始まる）を発行し、`Authorization: Bearer <キー>` で送ります。
  スコープは `read`（参照）・`write`（作成・更新・削除）・`admin`（管理。管理者のみ）で、対象のプロジェクトも限定できます。
- REST API の一覧: `GET /api/endpoints`。
- MCP: `POST /mcp`（Streamable HTTP）。例（Claude Code）:

```bash
claude mcp add --transport http skea https://skea.example.com/mcp --header "Authorization: Bearer skea_xxxxxxxx"
```

MCP のツール: `search_tasks`・`get_task`・`create_task`・`update_task`・`delete_task`・`add_comment`・`log_work`・`list_worklogs`・`get_board`・
`get_project`・`list_projects`・`list_notifications`・`get_me`、および画面でできるすべての操作のための `list_api_endpoints`・`call_api`。

## 連携の設定

- **GitHub / GitLab / Bitbucket / Forgejo**: プロジェクトの「設定 → リポジトリ連携」でリポジトリを追加し、表示された Webhook の URL と秘密の値を
  リポジトリの Webhook（push イベント、JSON）に設定します。コミットメッセージに `KEY-123` を書くとタスクにリンクされます。
  Webhook を使えない場合はアクセストークンを登録すると5分ごとにコミットを取得します。
  「サービス全体（すべてのリポジトリ）」を選ぶと、組織・グループ・サーバー全体の Webhook で、すべてのリポジトリの push を受け取ります。
- **Webhook（送信）**: 「設定 → Webhook」。本文の HMAC-SHA256 を `X-Skea-Signature: sha256=…` に付けます。
  「送信内容の見本」でイベントごとの JSON の例を、「送信履歴」で実際に送った本文を見られます。
- **skiwi など外部のサービス**: `OAUTH_CLIENTS` に登録すると、そのサービスが OAuth 2.0（認可コード + PKCE）で、
  ユーザーごとに読み取り専用のトークンを受け取れます（`/oauth/authorize`・`/oauth/token`・`/oauth/revoke`）。
  トークンは「<サービス名> 連携」という API キーとして個人設定に表示され、そこから失効できます。
  タスクIDのタイトルと状態は `POST /api/tasks/lookup {"keys":[…]}` でまとめて読めます。
- **skiwi のページのタイトル**: `SKIWI_*` を設定すると、各ユーザーが個人設定の「skiwi と連携する」で許可したあと、
  説明・コメント・リンクに貼った skiwi のページの URL に、その人の skiwi の権限でタイトルを出します。
- **skawau のファイル名**: `SKAWAU_*` を設定すると、同じように個人設定の「skawau と連携する」で許可したあと、
  貼った skawau のファイル・フォルダの URL に、その人の skawau の権限でファイル名を出します。
- **自動化**: 「設定 → 自動化」。テンプレートから作れます。書き方は画面の「書き方」と [docs/design.md](docs/design.md) を参照してください。
