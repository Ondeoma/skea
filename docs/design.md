# skea の設計

skea はチーム向けのタスク管理システムです（プロジェクト・タスク・スプリント・カンバン・ガントチャート）。
兄弟プロジェクト [skiwi](https://github.com/Ondeoma/skiwi)（Wiki）と同じ技術・同じ運用方針で作ります。

## 1. 要件サマリ

| 区分 | 内容 |
|---|---|
| テナント | シングルテナント（1つの組織で1つの skea） |
| 認証 | OIDC（Authorization Code + PKCE）。Cookieセッション、ユーザー単位のスコープ付きAPIキー |
| 権限 | システム権限（管理者・一般）と、プロジェクトごとの権限（管理・編集・閲覧）。管理者がユーザーごとに設定 |
| データ | プロジェクト → タスク（任意階層のサブタスク） |
| 画面 | カンバン（アクティブなスプリント）、バックログ、タスク一覧・検索、タスク詳細、ガントチャート、作業時間の日次一覧、通知一覧、設定 |
| 同期 | サーバーが変更を画面へ即座に知らせ（SSE）、画面は変更を取り直して通知・表示を更新（つながらないときは15秒ごとに確認） |
| コメント | リッチテキスト（WYSIWYG）、コメントごとのアンカーリンク、@メンション |
| 通知 | 画面内の通知（「メンションされたもの」などで絞り込み）、メール、Webhook |
| 自動化 | イベント・定期・手動・受信Webhookをトリガーに、条件分岐・API呼び出しを含むアクションを実行 |
| 検索 | PGroonga。日本語の部分一致、かな・全角半角の揺れ、タイプミスの許容（曖昧検索）、絞り込み条件の組み合わせ |
| 連携 | GitHub / GitLab / Bitbucket / Forgejo(Gitea)：コミットメッセージのタスクIDで自動リンク。skiwi・skawau：貼ったページ・ファイルの URL に名前を表示（OAuth） |
| AI | MCP（Model Context Protocol）。ユーザーが画面でできることはすべてAPI・MCPからもできる |
| 基盤 | PostgreSQL（Patroniで冗長化）、S3互換ストレージ（開発は ScalaS3）、アプリは複数ノードで動かせるステートレス構成 |
| 実装 | Scala 3.3 LTS、Cats Effect 3、http4s 1.0（マイルストーン版）、doobie、Flyway。画面は Scalactive（Scala.js）。リッチテキストは Tiptap |
| その他 | レスポンシブ対応、MITライセンス |

## 2. 全体アーキテクチャ

```
        Browser (Scalactive SPA + Tiptap)
           │ HTTPS（REST / MCP / 受信Webhook）
   ┌───────▼─────────┐   VIP(keepalived) + HAProxy
   │ node1 node2 node3 │ ← 各ノードで同一構成
   │  skea-app (Scala) │ http4s: 画面配信 / REST / MCP / Webhook受信
   │  Patroni + PG     │ PostgreSQL + PGroonga
   │  etcd             │ Patroni のコンセンサス
   │  S3互換ストレージ   │ 添付ファイル
   └───────────────────┘
        ▲ OIDC            ▲ Webhook（GitHub 等）    ▲ OAuth・名前の問い合わせ
       IdP               SCM                       skiwi・skawau
```

- アプリはステートレスです。状態はすべて PostgreSQL と S3互換ストレージに置きます（セッションもDB）。
- 画面の同期は「SSE の知らせ + 差分の取得」です。各ノードは1秒ごとに DB の新しい変更履歴と通知を確かめ、
  そのノードにつないでいる画面へ知らせるだけなので、ノード間の連絡や振り分けの親和性は不要で、ロードバランサーは単純なラウンドロビンで済みます。
- バックグラウンド処理（メール送信、Webhook送信、自動化、SCMのポーリング）は全ノードで動かし、
  対象行を `FOR UPDATE SKIP LOCKED` で取り合うため、二重実行しません。定期ジョブは `job_lock`（期限付きのリース）で1ノードだけが実行します。
- ノード間で共有する状態はすべて DB に置くため、ノード間の通信（Redis や LISTEN/NOTIFY）は使いません。

## 3. データモデル（PostgreSQL）

主なテーブル（詳細は `modules/db/src/main/resources/db/migration`）。

- `app_user(id, sub, name, email, avatar_url, is_admin, disabled, created_at, last_login_at)`
- `user_group(id, name)`, `group_member` … IdP の groups クレームから同期（`ADMIN_GROUPS` の所属者は管理者）
- `session(token_hash, user_id, expires_at)`
- `api_key(id, user_id, name, prefix, key_hash, scopes text[], project_ids uuid[], expires_at, last_used_at, revoked_at)`
- `project(id, key, name, description, lead_id, archived, task_seq, created_at, position)`
  - `key` は英大文字で始まる英数字（例 `SKEA`）。タスクIDは `SKEA-123`（`task_seq` で採番）。
- `project_member(project_id, user_id, role)` … `admin`（プロジェクト管理）/ `edit` / `view`
- `status(id, project_id, name, category, color, position)` … `category` は `todo`/`in_progress`/`waiting`/`done`。
  プロジェクト作成時に「作業前・進行中・待機中・完了」の4つを作る。名前・色・順序・追加・削除は変更可（カテゴリは固定の4種）。
- `label(id, project_id, name, color)`
- `sprint(id, project_id, name, goal, state, start_date, end_date, completed_at)` … `state` は `planned`/`active`/`closed`。アクティブは1プロジェクト1つ。
- `task(id, project_id, number, key, parent_id, title, description jsonb, description_text, status_id, sprint_id,
        assignee_id, reporter_id, start_date, due_date, rank, created_at, updated_at, deleted_at, version)`
  - `rank` は fractional index（カンバン・バックログでの並び順）。`version` は楽観ロック用。
  - `description` は Tiptap(ProseMirror) の JSON、`description_text` は検索用の平文。
- `task_label(task_id, label_id)`, `task_watcher(task_id, user_id)`
- `task_link(id, task_id, kind, target_task_id, url, title, created_by)` … タスク間（`relates`/`blocks`/`duplicates` 等）または任意URL
- `task_dependency(predecessor_id, successor_id, kind)` … ガント用（`fs`: 終了→開始）。循環は作成時に拒否。
- `milestone(id, project_id, name, date, color)` … ガントの目印（リリース日など。タスクには紐付かない）。
- `comment_draft(user_id, task_id, body jsonb, updated_at)` … 入力中のコメントの下書き（ユーザー＋タスクごとに1つ。投稿すると消す）。
- `comment_reaction(comment_id, user_id, emoji)` … 決まった絵文字か、`custom_emoji(name, s3_key)` に登録したカスタム絵文字（`:name:`）。
- `task.priority`（highest〜lowest）・`task.type`（task / bug / story / epic / improvement）。
- `task_template(id, project_id, user_id, name, title, description, status_id, assignee_id, label_ids, fields, subtasks, due_in_days, priority, task_type)` … タスクのテンプレート。
  project_id（プロジェクトで共有）か user_id（自分用）のどちらか一方を持つ。自分用はどのプロジェクトでも使えるため、プロジェクトに属する値（ステータス・担当者・ラベル・カスタムフィールド）は保存しない。
  自分用は本人だけが読み書きでき、繰り返しタスクには使えない。
- `recurring_task(id, project_id, template_id, frequency, weekday, monthday, active_sprint, next_run, enabled, last_run_at, last_task_key)` …
  テンプレートから決まった日にタスクを作る。5分ごとに1ノードだけが確かめ、作成者として作る。止まっていた間の分はまとめて1つだけ作る。
- `board_filter(id, project_id, user_id, name, cond)` … ボードの絞り込み条件（文字・担当者・ラベル・優先度・種類・期日）。名前を付けて保存し、ボードの上のボタンで呼び出す。
  user_id が空ならプロジェクトで共有する。自分用は閲覧権限で、共有は編集権限で保存できる。共有のものは作った人か管理者が消せる。
- `worklog(id, task_id, user_id, work_date, minutes, note, created_at)`
- `comment(id, task_id, author_id, body jsonb, body_text, created_at, updated_at, deleted_at)`
- `custom_field(id, project_id, name, kind, options jsonb, required, position)` … `kind` は `text`/`textarea`/`checkbox`/`radio`/`select`
- `task_field_value(task_id, field_id, value jsonb)`
- `attachment(id, task_id, comment_id, s3_key, file_name, size, content_type, sha256, created_by, created_at)`
- `task_event(id bigserial, project_id, task_id, actor_id, via, kind, detail jsonb, at)` … 変更履歴。画面の同期（`id` より新しいものを取得）にも使う。
- `notification(id, user_id, kind, task_id, comment_id, actor_id, title, body, at, read_at)` … `kind` は `mention`/`assigned`/`comment`/`updated`/`status`/`due`/`automation`
- `notify_pref(user_id, kind, email, chat)` … 通知の種類ごとのメール・チャット（stui）への送信可否
- `mail_outbox(id, user_id, subject, body, dedupe_key, not_before, attempts, sent_at, skipped, last_error)`
- `webhook(id, project_id, name, url, secret, events text[], enabled)`, `webhook_delivery(...)` … 送信のアウトボックス（再試行つき）
- `automation_rule(id, project_id, name, enabled, trigger jsonb, actions jsonb, run_as, created_by, incoming_token)`、`automation_run(...)`、`automation_queue(...)`
- `scm_repo(id, project_id, provider, name, web_url, api_url, secret, token_enc, last_polled_at)`、`task_commit(task_id, repo_id, sha, message, url, author, committed_at)`
- `integration_token(user_id, provider, token_enc)`、`integration_state(...)` … skiwi・skawau の OAuth トークン（暗号化）と認可の途中の state
- `job_lock(name, holder, expires_at)`, `audit_log(...)`

## 4. 認証・認可

- **ログイン**：OIDC（Authorization Code + PKCE）。ID token の `iss`/`aud`/`exp`/`nonce` を検証（skiwi と同じ）。
- **ユーザー**：初回ログインで作成。`ADMIN_GROUPS` の所属者、または画面で管理者にしたユーザーがシステム管理者。
  IdP 未設定の開発環境では `DEV_LOGIN=true` で名前だけのログインを許可する（本番では使わない）。
  ログインの state は、DB に加えてブラウザの Cookie にも入れて照合する。
- **CSRF**: Cookie のセッションでの更新系の要求は、Origin（なければ Referer）が `PUBLIC_URL` と同じ origin のときだけ受け付ける。
- **外部への送信（SSRF 対策）**: Webhook・自動化の Web リクエスト・リポジトリ連携は、名前解決したアドレスが内部（loopback・link-local・プライベート）なら送らない（`OUTBOUND_ALLOW_HOSTS` で例外）。
- **自動化の実行ユーザー**: 自分以外を指定できるのはシステム管理者だけ。実行中はシステム管理者の権限を使わず、編集できないタスク（ほかのプロジェクトのリンク先など）には触れない。
- **リクエストの上限**: 本文は 2MB（アップロードは `MAX_UPLOAD_BYTES`、受信 Webhook は 10MB）。リッチテキストは入れ子の深さも制限する。
- **APIキー**：`skea_` で始まるランダム値。ハッシュのみ保存。`Authorization: Bearer` で送る。
  - スコープ：`read`（参照のみ）、`write`（作成・更新・削除）、`admin`（管理操作。発行者が管理者のときだけ有効）。
  - プロジェクトの限定：指定したプロジェクトだけを操作できる（未指定なら本人が見られるすべて）。
  - 権限は「本人の権限 ∩ キーのスコープ」。キーでキーの発行・失効はできない。
- **外部のサービスへの認可**（skiwi との連携）：OAuth 2.0 の認可コード + PKCE（S256 のみ）。クライアントは `OAUTH_CLIENTS` で登録し、
  `redirect_uri` は完全一致で照合する。許可の画面はサーバーで組み立てる HTML（CSRF は SameSite=Strict の Cookie と hidden の値の一致で防ぐ）。
  認可コードは60秒・一度だけ（`oauth_code` にハッシュを保存）。トークンはそのユーザーのスコープ `read` の API キー（`api_key.oauth_client`）で、
  許可し直すと前のキーを失効させる。`/oauth/revoke`（RFC 7009）はそのクライアントが発行したキーだけを失効させる。
  `POST /api/tasks/lookup` は読み取り専用のキーでも呼べる（本文で条件を送るだけの読み取り）。
- **プロジェクト権限**：`view`（閲覧）< `edit`（タスク・コメントの作成・更新、スプリント操作）< `admin`（プロジェクト設定・メンバー）。
  システム管理者は全プロジェクトの `admin`。権限のないプロジェクトは存在を隠す（404）。

## 5. 画面の自動同期（SSE と差分の取得）

- 画面は `GET /api/stream?projectId={id}`（Server-Sent Events）につなぐ。サーバーはそのプロジェクトの変更（`change`）と、
  自分への通知（`notify`）があると知らせる（20秒ごとに空の行を送って接続を保つ）。
- 知らせを受けると（まとめて1回）`GET /api/projects/{id}/changes?since={eventId}` を呼び、新しい `task_event` を受け取る。
  SSE がつながらないときは15秒ごと、つながっている間も念のため60秒ごとに確かめる。
- 変化があれば「○○さんが SKEA-12 を更新しました」のトーストを出し、表示中のデータ（カンバン・タスク詳細など）を取り直す。
  編集中の入力欄は上書きしない（タスク詳細では「最新の内容があります」と出し、保存時は `version` で競合を検出）。
- タブが非表示のあいだは確認を止め、表示に戻ったときに1回確認する。通知の件数も同じ周期で取り直す。

## 6. コメント・説明（リッチテキスト）

- エディタは Tiptap（skiwi のエディタを簡略化したもの。同時編集・目次は持たない）。見出し、太字、斜体、取り消し線、コード、
  リスト、ToDo、引用、コードブロック（言語を選んでシンタックスハイライト。lowlight / highlight.js）、表（セルの背景色）、
  リンク、画像（貼り付けで添付として保存。大きさと寄せ方向）、@メンション、`/` で開くブロックの挿入メニュー。
- 色は色名（`yellow` など）だけを保存し、実際の色は CSS で決める（ダークモード対応）。画像の幅・寄せ方向・セルの色は許可した値だけを HTML にする。
- 入力中のコメントは、入力が止まって 1.5 秒後に `PUT /api/tasks/{id}/draft` で下書きとして保存し、画面を開き直すと復元する。
- タスクの画面は、変更の同期で詳細を読み直しても、コメント・説明の入力欄を作り直さない（表示中のタスクが変わったときだけ作り直す）。
- 保存形式は ProseMirror の JSON。サーバーは JSON を検証し、許可したノード・マークだけから HTML を組み立てる（XSS対策）。
  平文（検索用）とメンションされたユーザーもサーバーで JSON から取り出す。
- まとめてのメンション：メンションの `id` が `g:<グループID>` ならグループの人、`r:admin`・`r:edit`・`r:all` ならプロジェクトの役割の人に通知する
  （どちらもプロジェクトを閲覧できる人だけ）。候補は `GET /api/projects/{id}/mention-groups`。
- コメントのアンカー：`/p/{key}/tasks/{taskKey}#comment-{id}`。画面はそのコメントまでスクロールして強調する。

## 7. 通知

- **画面内**：`notification`。種類（メンション・担当・コメント・ウォッチ・期限・自動化）と未読で絞り込める。
- **メール**：`mail_outbox` に積み、ワーカーが SMTP で送る（失敗時は指数バックオフ、`dedupe_key` で集約、`SKIP LOCKED`）。
  送信時点で閲覧権限がない宛先には送らない。種類ごとにユーザーが無効にできる。
- **Webhook**：プロジェクトごとに登録。イベント（`task.created` など）を JSON で POST し、`X-Skea-Signature: sha256=<HMAC>` を付ける。
  `webhook_delivery` をアウトボックスとして再試行する。
- 宛先：担当者・報告者・ウォッチャー（操作した本人は除く）。メンションは本文に含まれた人。

## 8. 自動化（オートメーション）

- ルール ＝ トリガー ＋ アクションの列（JSON で保存）。
- **トリガー**：`task_created`、`task_updated`（フィールド指定可）、`status_changed`（遷移元・遷移先）、`comment_added`、
  `sprint_started`、`sprint_completed`、`commit_linked`、`scheduled`（毎日の時刻・間隔）、`manual`（タスク画面のボタン）、
  `incoming_webhook`（ルールごとの秘密のURL）。
- **条件**：`all`/`any`/`not` の組み合わせと、フィールドの比較（`eq`/`neq`/`contains`/`in`/`empty`/`gt`/`lt` など）。
- **アクション**：フィールドの設定、ステータスの変更、担当者の設定、ラベル・ウォッチャーの追加、コメント、サブタスクの作成、
  タスクの作成、メール送信、Web リクエスト（メソッド・ヘッダー・本文。応答を変数 `{{webResponse}}` に入れる）、
  変数の設定、**条件分岐（if / else）**、ログ出力。
- **スマート値**：`{{task.key}}`、`{{task.title}}`、`{{task.status}}`、`{{task.assignee.name}}`、`{{actor.name}}`、`{{webResponse.body.xxx}}` など。
- 実行はキュー（`automation_queue`）経由で非同期。ルールによる変更が別のルールを起動する連鎖は深さ5まで。実行結果は `automation_run` に記録する。

## 9. 検索（日本語の曖昧検索）

- PGroonga のインデックスを `task(title, description_text)` と `comment.body_text` に作る。
  - トークナイザ `TokenNgram`（`unify_alphabet`/`unify_digit`/`loose_symbol`）、ノーマライザ `NormalizerNFKC150`
    （`unify_kana`（ひらがな・カタカナ）、`unify_kana_case`（小書き）、`unify_hyphen_and_prolonged_sound_mark`、`unify_middle_dot` 等）。
  - タイプミスの許容：`pgroonga_condition(..., fuzzy_max_distance_ratio => 0.34)`。
- 検索語は空白区切りで AND。検索構文の注入を防ぐため、各語はフレーズとしてエスケープする。
- **絞り込み構文**（検索欄にそのまま書ける）：`project:SKEA status:進行中 assignee:me reporter:山田 label:バグ sprint:active
  due<2026-12-01 updated>-7d is:open has:attachment parent:SKEA-1 field:顧客=A社 order:due`。画面の絞り込みUIも同じ構文を組み立てる。
- 結果はタイトル一致を優先し、抜粋（`pgroonga_snippet_html`）を付ける。閲覧できるプロジェクトだけに絞る。

## 10. ソースコード管理（SCM）との連携

- プロジェクトにリポジトリを登録する（GitHub / GitLab / Bitbucket / Forgejo・Gitea）。
- **Webhook**：`POST /hooks/scm/{repoId}`。署名を検証する（GitHub/Forgejo/Gitea/Bitbucket：HMAC-SHA256、GitLab：`X-Gitlab-Token`）。
  push イベントの各コミットのメッセージから `KEY-123` を取り出し、そのプロジェクトのタスクに `task_commit` としてリンクする。
- **ポーリング**（任意）：アクセストークンを登録したリポジトリは、5分ごとに各サービスの REST API で新しいコミットを取得する（Webhook を設定できない環境向け）。
- **一括監視**（`all_repos`）：サービス全体（組織・グループ・サーバー）の Webhook を1つの登録で受ける。リポジトリ名は push の本文
  （`repository.full_name`、GitLab は `project.path_with_namespace`）から取り、`task_commit.repo_name` に保存する。
  名前に組織名などを入れると、その配下のリポジトリだけを対象にする。ポーリングでは最近更新された20件のリポジトリを確認する。
- リンクしたときは `commit_linked` イベント（自動化のトリガー）を出す。

## 11. skiwi・skawau・stui との連携

各ユーザーが個人設定で skiwi・skawau を OAuth 2.0（認可コード + PKCE）で許可すると、説明・コメント・リンクに貼った
skiwi のページ・skawau のファイルの URL に、その人の権限でタイトル・ファイル名を表示する。トークンは `APP_SECRET` から作る鍵で AES-GCM 暗号化して保存する。
取り決めは [skiwi-integration.md](skiwi-integration.md)・[skawau-integration.md](skawau-integration.md)。

stui（チャット）とも同じ仕組みで連携し、許可したユーザーには skea の通知を stui の DM でも送る（[stui-integration.md](stui-integration.md)）。

## 12. MCP

- `POST /mcp`（Streamable HTTP、JSON 応答）。APIキー（Bearer）だけを受け付ける。
- 主要な操作は専用のツール（`search_tasks`、`get_task`、`create_task`、`update_task`、`add_comment`、`log_work`、`list_sprints` など）。
- **「ユーザーができることはすべて」を保証するため、`list_api_endpoints`（REST API の一覧）と `call_api`（任意の REST API の呼び出し）も提供する。**
  どのツールも内部で REST API を同じキーで呼ぶので、権限・スコープ・監査は REST API と同じになる。

## 13. 冗長構成・デプロイ

- skiwi と同じく、各ノードに `skea-app`、Patroni + PostgreSQL(+PGroonga)、etcd。入口は keepalived（VIP）と HAProxy。
- DB の URL は複数ホスト + `targetServerType=primary`。HikariCP の接続確認を短くし、フェイルオーバー後に張り直す。
- `/healthz` は DB のプライマリに書けるときだけ 200（HAProxy のヘルスチェック用）。
- 起動時のマイグレーションは Flyway のロックで1回だけ適用される（複数ノード同時起動でも安全）。
- 無停止更新は1ノードずつ入れ替える（SSE はブラウザが自動でつなぎ直し、取りこぼしは差分の取得で埋まる）。
- SSE はプロキシでバッファしないこと（`X-Accel-Buffering: no` を付けている。HAProxy は `timeout tunnel` を長めにする）。

## 14. sbt モジュール構成

```
skea
├─ shared     バックエンドと画面(Scala.js)で共有する API の型（circe）
├─ core       ドメイン（権限、タスクID、絞り込み構文、自動化の評価、リッチテキストの変換、fractional index）
├─ db         doobie のストアと Flyway マイグレーション、ジョブのリース、送信待ちの表の処理
├─ auth       OIDC クライアント、暗号（トークン、AES-GCM）
├─ storage    S3互換ストレージ
├─ notify     メール（SMTP）
├─ integrations  SCM（Webhook の解析・各サービスの API）
├─ api        http4s のルート（REST、MCP、受信Webhook）とサービス（通知・自動化・Webhook送信）
├─ app        起動・設定（環境変数）・画面の配信
└─ web        Scalactive(Scala.js)で書いた画面
frontend/     Vite。web の出力と Tiptap をまとめてビルドし、app が配信する
```
