<script lang="scala">
import skea.web.*
import skea.web.parts.*
import io.circe.Json
import io.circe.syntax.*
import skea.shared.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// ソースコード管理との連携（GitHub / GitLab / Bitbucket / Forgejo・Gitea）
case class State(repos: RV[List[RepoDto]] = RV(Nil),
                 editing: RV[Option[Option[RepoDto]]] = RV(None),
                 provider: RV[String] = RV("github"),
                 name: RV[String] = RV(""),
                 webUrl: RV[String] = RV(""),
                 apiUrl: RV[String] = RV(""),
                 branch: RV[String] = RV(""),
                 token: RV[String] = RV(""),
                 allRepos: RV[Boolean] = RV(false))

def initState() = State()


def load(s: State): Unit = Api.get[List[RepoDto]](s"/api/projects/${Store.pid}/repos").foreach(s.repos := _)

def mounted(s: State): Unit = load(s)

def edit(s: State, r: Option[RepoDto]): Unit = {
  s.provider := r.map(_.provider).getOrElse("github"); s.name := r.map(_.name).getOrElse("")
  s.webUrl := r.map(_.webUrl).getOrElse(""); s.apiUrl := r.flatMap(_.apiUrl).getOrElse("")
  s.branch := r.flatMap(_.branch).getOrElse(""); s.token := ""; s.allRepos := r.exists(_.allRepos); s.editing := Some(r)
}

def save(s: State): Unit = {
  val r = RepoReq(s.provider.v, s.name.v.trim, s.webUrl.v.trim, Option(s.apiUrl.v.trim).filter(_.nonEmpty), Option(s.branch.v.trim).filter(_.nonEmpty),
    Option(s.token.v.trim).filter(_.nonEmpty), Some(s.allRepos.v))
  val f = s.editing.v.flatten match {
    case Some(x) => Api.putUnit(s"/api/repos/${x.id}", r.asJson)
    case None    => Api.post[RepoDto](s"/api/projects/${Store.pid}/repos", r.asJson).map(_ => ())
  }
  Toasts.report(f) { _ => s.editing := None; load(s) }
}

def remove(s: State, r: RepoDto): Unit =
  Dialog.confirm(s"「${r.name}」の連携をやめますか？（リンク済みのコミットも消えます）") { Toasts.report(Api.delete(s"/api/repos/${r.id}"))(_ => load(s)) }

def poll(s: State, r: RepoDto): Unit = Toasts.report(Api.post[Json](s"/api/repos/${r.id}/poll")) { j =>
  Toasts.show(s"${j.hcursor.get[Int]("linked").getOrElse(0)} 件のコミットをリンクしました"); load(s); Store.changed()
}

/** サービスの既定の URL（一括監視のとき）. */
def serviceUrl(p: String): String = p match {
  case "github" => "https://github.com"; case "gitlab" => "https://gitlab.com"; case "bitbucket" => "https://bitbucket.org"; case _ => "https://"
}

def setMode(s: State, all: Boolean): Unit = {
  s.allRepos := all
  if (all && (s.webUrl.v.isEmpty || s.webUrl.v.count(_ == '/') > 2)) s.webUrl := serviceUrl(s.provider.v)
}

def title(r: RepoDto): String = if (r.allRepos) (if (r.name.isEmpty) "すべてのリポジトリ" else s"${r.name} のすべてのリポジトリ") else r.name
def hookHelp(r: RepoDto): String =
  if (!r.allRepos) "リポジトリの Webhook（push イベント）に設定してください。"
  else r.provider match {
    case "github"    => "組織（Organization）の Settings → Webhooks に設定すると、組織のすべてのリポジトリの push を受け取ります。"
    case "gitlab"    => "グループの Webhook（Premium）か、管理者の System hooks に設定してください。"
    case "bitbucket" => "ワークスペースの Webhook に設定してください。"
    case _           => "組織の Webhook か、管理者の「既定の Webhook / システムの Webhook」に設定してください。"
  }
def mode(s: State, all: Boolean) = s.allRepos.mapC(a => if (a == all) "chip active" else "chip")

def label(p: String): String = p match { case "github" => "GitHub"; case "gitlab" => "GitLab"; case "bitbucket" => "Bitbucket"; case _ => "Forgejo / Gitea" }
def key = Store.meta.mapC(_.map(_.project.key).getOrElse("KEY"))
def secretHelp(p: String): String = p match {
  case "gitlab" => "Secret token に設定"
  case "bitbucket" => "Secret に設定（HMAC-SHA256）"
  case _ => "Secret に設定（Content type: application/json）"
}
</script>

<template>
  <section>
    <div class="row"><h2>リポジトリ連携</h2><span class="grow"></span><button v-if="Store.isProjectAdmin" class="primary small" @click="edit(s, None)">＋ リポジトリを追加</button></div>
    <p class="muted small">コミットメッセージに <code>{{ key }}-123</code> のようなタスクIDを書くと、そのタスクにコミットが自動でリンクされます。
      リポジトリ側に下の Webhook（push イベント）を設定してください。Webhook を設定できない場合は、アクセストークンを登録すると5分ごとにコミットを取得します。</p>
    <div v-for="r in s.repos" :key="r.id" class="panel">
      <div class="row"><b>{{ label(r.provider) }}</b><a :href="Fmt.href(r.webUrl)" target="_blank" rel="noopener">{{ title(r) }}</a>
        <span v-if="r.allRepos" class="badge">一括監視</span>
        <span v-if="r.branch.isDefined" class="badge">{{ r.branch.getOrElse("") }}</span><span class="grow"></span>
        <button v-if="r.hasToken" class="small" @click="poll(s, r)">今すぐ取得</button>
        <button v-if="Store.isProjectAdmin" class="ghost small" @click="edit(s, Some(r))">編集</button><button v-if="Store.isProjectAdmin" class="ghost small" @click="remove(s, r)">削除</button></div>
      <div v-if="Store.isProjectAdmin" class="small hook">
        <div class="muted">{{ hookHelp(r) }}</div>
        <div>Webhook の URL: <code>{{ r.hookUrl }}</code> <button class="ghost small" @click="Dom.copy(r.hookUrl)">コピー</button></div>
        <div>秘密の値（{{ secretHelp(r.provider) }}）: <code>{{ r.secret }}</code> <button class="ghost small" @click="Dom.copy(r.secret)">コピー</button></div>
      </div>
      <div class="small muted">{{ if (r.hasToken) "ポーリング: 有効" + r.lastPolledAt.map(t => "（前回 " + Fmt.ago(t) + "）").getOrElse("") else "ポーリング: なし（Webhook のみ）" }}</div>
    </div>
    <p v-if="s.repos.mapC(_.isEmpty)" class="muted">連携しているリポジトリはありません</p>
    <Modal v-if="s.editing.mapC(_.isDefined)" :onClose="_ => s.editing := None">
      <form @submit.prevent="save(s)">
          <h2>リポジトリ</h2>
          <div class="field"><span>サービス</span>
            <Combo :bind="Some(s.provider)" :options='Opt.of(List("github" -> "GitHub", "gitlab" -> "GitLab", "bitbucket" -> "Bitbucket", "forgejo" -> "Forgejo / Gitea"))' /></div>
          <div class="field"><span>監視する範囲</span>
            <div class="chips">
              <button type="button" :class="mode(s, false)" @click="setMode(s, false)">1つのリポジトリ</button>
              <button type="button" :class="mode(s, true)" @click="setMode(s, true)">サービス全体（すべてのリポジトリ）</button>
            </div>
          </div>
          <template v-if="s.allRepos.mapC(!_)">
            <label class="field"><span>リポジトリ名（owner/repo。GitLab はグループを含むパス）</span><input type="text" v-model="s.name" placeholder="Ondeoma/skea"></label>
            <label class="field"><span>リポジトリの URL</span><input type="url" v-model="s.webUrl" placeholder="https://github.com/Ondeoma/skea"></label>
          </template>
          <template v-if="s.allRepos">
            <p class="muted small">組織・グループ・サーバー全体の Webhook で、すべてのリポジトリの push を受け取ります。
              コミットメッセージにこのプロジェクトのタスクID があれば、どのリポジトリのコミットでもリンクします。</p>
            <label class="field"><span>組織・ユーザー・グループ名（空ならすべて）</span><input type="text" v-model="s.name" placeholder="Ondeoma"></label>
            <label class="field"><span>サービスの URL</span><input type="url" v-model="s.webUrl" placeholder="https://github.com"></label>
          </template>
          <label class="field"><span>API の URL（自前のサーバーで既定と違う場合のみ）</span><input type="url" v-model="s.apiUrl"></label>
          <label v-if="s.allRepos.mapC(!_)" class="field"><span>ブランチ（ポーリングの対象。空なら既定のブランチ）</span><input type="text" v-model="s.branch"></label>
          <label class="field"><span>アクセストークン（ポーリングする場合。読み取り権限だけで十分。一括監視では最近更新された20件のリポジトリを確認します。保存後は表示されません）</span><input type="password" v-model="s.token" autocomplete="new-password"></label>
          <div class="actions"><button type="button" @click="s.editing := None">キャンセル</button><button type="submit" class="primary">保存</button></div>
        </form>
    </Modal>
  </section>
</template>

<style scoped>
.hook { margin: .4rem 0; }
.hook code { word-break: break-all; }
</style>
