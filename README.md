# skea

<p align="center"><img src="frontend/public/logo.svg" width="128" height="128" alt="skea のロゴ"></p>

skea は、チームのためのタスク管理システムです（プロジェクト・タスク・スプリント・カンバン・ガントチャート）。ライセンス: MIT

- **カンバン**（アクティブなスプリントのタスクをステータスの列に表示。ドラッグで移動し、落とす位置が空いて並び替わる様子が分かる。
  担当者・ラベル・優先度・種類・期日での絞り込み、絞り込み条件に名前を付けて保存しボタンで呼び出す（自分用・プロジェクトで共有）、担当者・ラベルごとのスイムレーン、クリックで詳細をモーダル表示。タッチ端末では長押しでドラッグ）、**バックログ**、**一覧・検索**（担当者・ステータス・日付などの条件の欄）、
  **ガントチャート**（依存関係をバーの右端からドラッグでつなぐ・矢印のクリックで外す、クリティカルパスの強調、
  先行タスクを動かすと後続も一緒にずらす、マイルストーン、任意階層のサブタスク）
- **タスク**: タスクID（`SKEA-123`）、タイトル、説明（リッチテキスト）、期日・開始日、ラベル、スプリント、ステータス（作業前・進行中・待機中・完了。追加・変更可）、
  リンク（別タスク・URL。skiwi のページ・skawau のファイルは名前を表示）、担当者、報告者、ウォッチャー（ほかの人も追加できる）、作業時間、添付ファイル、
  カスタムフィールド（テキスト・複数行・チェックボックス・ラジオ・選択肢）
- **コメント**: WYSIWYG（Tiptap）、@メンション、`/` のメニュー、画像の貼り付け（大きさ・寄せ方向）、表（セルの背景色）、
  言語を選べるコードブロック（シンタックスハイライト）、コメントごとのアンカーリンク、入力中の下書きの自動保存（ユーザー＋タスクごと）、
  絵文字のリアクション（カスタム絵文字も登録できる）、人・グループ・役割（管理者・編集者・全員）へのメンション
- **テンプレート・繰り返しタスク**: よく作るタスクの初期値とサブタスクをテンプレートにし、毎日・毎週・毎月の決まった日に自動で作る。テンプレートはプロジェクトで共有するものと、自分用（個人設定で管理し、どのプロジェクトでも使える）に分けて保存でき、タスクの作成画面の入力内容からも保存できる
- **取り込み**: CSV や Jira の書き出し（CSV）から、ステータス・担当者・ラベル・親子関係を対応付けて取り込む
- **ユーザー**: アイコンの画像を登録できる（IdP の画像より優先）。画面の色は自動（OS に合わせる）・ライト・ダークを選べる
- **タスクの優先度と種類**: 優先度（最高〜最低）と種類（タスク・バグ・ストーリー・エピック・改善）。カード・一覧・検索（`priority:high type:bug`）で使える
- **一覧の表示の切り替え**: 表示する列の選択、ステータス・担当者・優先度などでのグループ化、一覧の中での直接の編集
- **自動同期**: 変更はサーバーから画面へ即座に知らせ（Server-Sent Events）、ほかの人の変更を通知して表示を更新（つながらないときは15秒ごとに確認）
- **通知**: 画面内（メンション・担当・コメントなどで絞り込み）、メール、Webhook、stui のチャンネル（OAuth の連携で、URL なしで設定できる）
- **自動化**: イベント・定期・手動・受信 Webhook をきっかけに、条件分岐・Web API の呼び出しを含むアクションを実行
- **検索**: 日本語の全文検索（PGroonga）。かな・全角半角の揺れ、入力ミスを許す曖昧検索、`status:進行中 assignee:me due<+7d` のような絞り込み
- **作業時間の一覧**: 誰が日ごとにどのタスクを何時間したか（人×日、メンバーを選んでタスク×日、日ごとの明細）
- **連携**: GitHub / GitLab / Bitbucket / Forgejo（コミットメッセージのタスクIDで自動リンク）、skiwi（ページのタイトル）・skawau（ファイル名）
- **API・AI**: ユーザーごとのスコープ付き APIキー、REST API、MCP（AI から画面と同じ操作ができる）
- **認証・権限**: OIDC、システム管理者・一般、プロジェクトごとの権限（管理・編集・閲覧。ユーザー・IdP のグループ単位）
- **ヘルプデスク**: プロジェクトをヘルプデスクにすると、問い合わせする「ユーザ」と回答する「エージェント」に分かれる。
  ユーザは簡素な窓口の画面（`/help/KEY`）で問い合わせ・返信し、自分の問い合わせと共有されたタスクだけが見える。
  エージェントはユーザに見えない内部メモを書け、タスクをユーザのグループ（親子の階層。親に共有すると子にも見える）や個人に共有・通知できる。
  窓口から OIDC でログインした人が自分で登録するゲスト登録（メールのドメインで制限）。
  窓口の検索は、キーワード（件名・内容・公開の返信。曖昧検索）と `status:進行中 is:mine created>-30d order:-updated` のような絞り込み、
  入力欄で条件を組み立てる詳細条件を使える（見えるのは自分に見える問い合わせの公開の部分だけ）
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
| `OIDC_ISSUER` / `OIDC_CLIENT_ID` / `OIDC_CLIENT_SECRET` | OIDC。Redirect URI は `${PUBLIC_URL}/auth/callback`。ログアウトでは IdP からもログアウトし、`${PUBLIC_URL}/` に戻る（sruru 以外の IdP では、この戻り先も登録する） |
| `OIDC_GROUPS_CLAIM` | 所属グループのクレーム名（既定 `groups`） |
| `ADMIN_GROUPS` | システム管理者にするグループ（カンマ区切り）。画面からも管理者を付けられる |
| `APP_SECRET` | 秘密の値の暗号化（SCM のトークン・外部サービスの OAuth トークン）に使う16文字以上の値 |
| `STATIC_DIR` | ビルド済みの画面（`dist/static`） |
| `S3_ENDPOINT` / `S3_BUCKET` / `S3_ACCESS_KEY` / `S3_SECRET_KEY` | 添付ファイル（未設定なら添付は使えない） |
| `SMTP_HOST` / `SMTP_PORT` / `SMTP_TLS` / `SMTP_USER` / `SMTP_PASSWORD` / `MAIL_FROM` | メール（未設定ならメールは送らない） |
| `MAX_UPLOAD_MB` | 添付の上限（既定 100） |
| `TIMEZONE` | 期限の通知などの「今日」（既定 Asia/Tokyo） |
| `WEBHOOK_ALLOW_HTTP` | Webhook の送信先に http:// を許す（開発用） |
| `OUTBOUND_ALLOW_HOSTS` | Webhook・自動化の Web リクエスト・リポジトリ連携で、内部のアドレスでも送ってよいホスト名（カンマ区切り）。既定では loopback・プライベート・link-local のアドレスへは送らない |
| `OUTBOUND_ALLOW_PRIVATE` | `true` でプライベートのアドレス（10.0.0.0/8 など）への送信を許す（loopback・link-local は許さない） |
| `AUTOMATION_MAIL_DOMAINS` | 自動化のメールで送ってよい外部のドメイン（カンマ区切り）。既定では登録しているユーザーのアドレスだけ |
| `SKIWI_URL`・`SKIWI_CLIENT_ID`・`SKIWI_CLIENT_SECRET` | skiwi のページのタイトルを出す連携（skiwi に skea をクライアントとして登録した値。[docs/skiwi-integration.md](docs/skiwi-integration.md)） |
| `SKAWAU_URL`・`SKAWAU_CLIENT_ID`・`SKAWAU_CLIENT_SECRET` | skawau のファイル名を出す連携（skawau に skea をクライアントとして登録した値。[docs/skawau-integration.md](docs/skawau-integration.md)） |
| `STUI_URL`・`STUI_CLIENT_ID`・`STUI_CLIENT_SECRET` | stui（チャット）に通知を送る連携（stui に skea をクライアントとして登録した値。[docs/stui-integration.md](docs/stui-integration.md)） |
| `SKIWI_INTERNAL_URL`・`SKAWAU_INTERNAL_URL`・`STUI_INTERNAL_URL` | サーバーから連携先を呼ぶときの URL（省略時は `*_URL`。同じネットワークの中の URL を使う場合） |
| `OAUTH_CLIENTS` | 認可を求める外部のサービス（skiwi・skakapo など）。`[{"id":"skiwi","secret":"…（16文字以上）","redirectUri":"https://…/callback","name":"skiwi"}]`。書き込みも許すなら `"scopes":["read","write"]`（skakapo のエージェントが起票・コメントするとき. 省くと read だけ） |
| `NODE_ID` | ノード名（ログ・定期処理のリース） |
| `OTEL_EXPORTER_OTLP_ENDPOINT` / `OTEL_EXPORTER_OTLP_HEADERS` | ログ・メトリクス・トレースの送り先（skorimako の URL と `Authorization=Bearer sko_…`（ingest のキー））。未設定なら送らない。そのほか OpenTelemetry の標準の `OTEL_*` が使える |
| `LOG_FORMAT` / `LOG_LEVEL` | 標準出力のログの形（`text`（既定）・`json`）と水準（既定 `INFO`）。ログにはトレースの ID と操作したユーザーが付く |
| `SKORIMAKO_RUM_TOKEN` / `SKORIMAKO_RUM_URL` / `SKORIMAKO_RUM_SAMPLE_RATE` | 画面のブラウザの観測（skorimako の RUM）。トークンは skorimako の管理の画面で登録したアプリのもの。URL を省略すると `SKORIMAKO_URL` |
| `VAPID_SUBJECT` | プッシュ通知（PWA）の連絡先（`mailto:…` か `https://…`。省略すると `PUBLIC_URL`）。VAPID の鍵は初回の起動で作って DB に保存する |
| `RATE_LIMIT_AUTH` / `RATE_LIMIT_PUBLIC` | IP ごとの1分あたりの回数の上限。ログインと OAuth の認可の画面（`/auth/…`・`/oauth/authorize`、既定 20）と、OAuth のトークンの発行・失効（`/oauth/token`・`/oauth/revoke`）（既定 120）。0 なら制限しない。ノードごとに数える。超えたら 429（`SKEA-1028`）と `Retry-After` |
| `TRUSTED_PROXIES` | リバースプロキシの IP か CIDR（カンマ区切り）。ここから来た要求だけ `X-Forwarded-For` を信じて、回数を数える IP を決める。リバースプロキシの後ろに置くときは必ず設定する（未設定だと、全員がプロキシの IP として数えられる） |
| `DEV_LOGIN` | 開発用のログインを有効にする（OIDC を設定せず、`PUBLIC_URL` が localhost〈`localhost`・`127.0.0.1`・`[::1]`・`*.localhost`〉の場合のみ。それ以外の `PUBLIC_URL` で設定すると起動しない） |

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

### エージェント専用アカウント（skakapo などの AI エージェント用）

外部の AI エージェントのサービス（skakapo など）に skea を使わせるための、ログインしないアカウントです。
個人の APIキーと違い、特定の人の権限ではなく、このアカウント自身の権限で動きます。エージェントが行った作成・変更・コメントは、このアカウントの名前で記録されます。

1. 管理者が「管理」→「エージェント」で、アカウントを作ります（例: `skakapo`）。
2. 見せたい・書き込ませたいプロジェクトの「設定 → メンバー」で、ふつうのユーザーと同じようにこのアカウントをメンバーに追加します（閲覧・編集など）。
3. 「キーを発行」でスコープ（`read`・`write`。`admin` は付けられません）と有効期限を選んで発行し、表示されたキーを
   skakapo の管理画面の「サービス」→ skea →「エージェント専用アカウント」に貼り付けます。キーは発行したときに一度だけ表示されます。
4. 使わなくなったら、キーを失効させるか、アカウントを無効にします（無効にすると、そのアカウントのキーはすべて使えなくなります）。

- アカウントの `sub` は `agent:<uuid>`、メールは持ちません。OIDC・開発用のログインではこのアカウントになれません。
- 管理の API（`/api/admin/agents`）は管理者が画面から（Cookie のセッションで）使うときだけ使えます。APIキーでは管理できません。
  - `GET /api/admin/agents`・`POST /api/admin/agents {name}`・`PATCH /api/admin/agents/{id} {name?, disabled?}`
  - `POST /api/admin/agents/{id}/keys {name, scopes, projectIds?, expiresInDays?}`・`DELETE /api/admin/agents/{id}/keys/{keyId}`
- 操作は監査ログに `agent.create`・`agent.update`・`agent.key.create`・`agent.key.revoke` として残ります。

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
- **stui への通知**: `STUI_*` を設定すると、個人設定の「stui と連携する」で許可したユーザーには、
  skea の通知（担当・メンション・コメント・期日など）が stui のボット「skea」からの DM でも届きます（送る種類は個人設定で選べます）。
- **自動化**: 「設定 → 自動化」。テンプレートから作れます。書き方は画面の「書き方」と [docs/design.md](docs/design.md) を参照してください。
