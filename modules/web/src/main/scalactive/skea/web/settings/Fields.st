<script lang="scala">
import skea.web.*
import skea.web.parts.*
import io.circe.syntax.*
import skea.shared.*


// カスタムフィールド（テキスト・複数行テキスト・チェックボックス・ラジオボタン・選択肢）
case class State(editing: RV[Option[Option[CustomFieldDto]]] = RV(None),
                 name: RV[String] = RV(""),
                 kind: RV[String] = RV(FieldKind.Text),
                 options: RV[String] = RV(""),
                 required: RV[Boolean] = RV(false),
                 helpForm: RV[Boolean] = RV(false),
                 helpHint: RV[String] = RV(""))

def initState() = State()

def fields = Store.meta.mapC(_.map(_.fields).getOrElse(Nil))

def edit(s: State, f: Option[CustomFieldDto]): Unit = {
  s.name := f.map(_.name).getOrElse("")
  s.kind := f.map(_.kind).getOrElse(FieldKind.Text)
  s.options := f.map(_.options.mkString("\n")).getOrElse("")
  s.required := f.exists(_.required)
  s.helpForm := f.exists(_.helpForm)
  s.helpHint := f.map(_.helpHint).getOrElse("")
  s.editing := Some(f)
}

def save(s: State): Unit = {
  val r = CustomFieldReq(s.name.v, s.kind.v, s.options.v.split("\n").toList.map(_.trim).filter(_.nonEmpty), s.required.v,
    Some(s.helpForm.v), Some(s.helpHint.v))
  val f = s.editing.v.flatten match {
    case Some(x) => Api.putUnit(s"/api/projects/${Store.pid}/fields/${x.id}", r.asJson)
    case None    => Api.post[CustomFieldDto](s"/api/projects/${Store.pid}/fields", r.asJson).map(_ => ())(scala.scalajs.concurrent.JSExecutionContext.Implicits.queue)
  }
  Toasts.report(f) { _ => s.editing := None; Store.reloadMeta() }
}

def remove(f: CustomFieldDto): Unit =
  Dialog.confirm(s"フィールド「${f.name}」を削除しますか？入力済みの値も消えます。") {
    Toasts.report(Api.delete(s"/api/projects/${Store.pid}/fields/${f.id}"))(_ => Store.reloadMeta())
  }

def hasOptions(s: State) = s.kind.mapC(FieldKind.hasOptions)
</script>

<template>
  <section>
    <div class="row"><h2>カスタムフィールド</h2><span class="grow"></span><button v-if="Store.isProjectAdmin" class="small" @click="edit(s, None)">＋ 追加</button></div>
    <div class="table-wrap">
      <table class="list">
        <thead><tr><th>名前</th><th>種類</th><th>選択肢</th><th v-if="Store.helpdesk">窓口</th><th></th></tr></thead>
        <tbody>
          <tr v-for="f in fields" :key="f.id">
            <td>{{ f.name }}{{ if (f.required) " *" else "" }}</td><td>{{ FieldKind.label(f.kind) }}</td><td class="small muted">{{ f.options.mkString(", ") }}</td>
            <td v-if="Store.helpdesk" class="small">{{ if (f.helpForm) "フォームに出す" else "" }}</td>
            <td class="nowrap"><button v-if="Store.isProjectAdmin" class="ghost small" @click="edit(s, Some(f))">編集</button><button v-if="Store.isProjectAdmin" class="ghost small" @click="remove(f)">削除</button></td>
          </tr>
          <tr v-if="fields.mapC(_.isEmpty)"><td colspan="4" class="muted">カスタムフィールドはありません</td></tr>
        </tbody>
      </table>
    </div>
    <p v-if="Store.helpdesk" class="muted small">ヘルプデスク: 「窓口のフォームに出す」にした項目は、窓口の「新しい問い合わせ」でユーザが入力し、問い合わせの画面でユーザにも見えます（並び順・必須もこの設定のとおり）。</p>
    <p class="muted small">検索では <code>field:名前=値</code> で絞り込めます。自動化では <code>&#123;&#123;task.fields.名前&#125;&#125;</code> で参照できます。</p>
    <Modal v-if="s.editing.mapC(_.isDefined)" :onClose="_ => s.editing := None">
      <form @submit.prevent="save(s)">
          <h2>カスタムフィールド</h2>
          <label class="field"><span>名前</span><input type="text" v-model="s.name"></label>
          <div class="field"><span>種類</span><Combo :bind="Some(s.kind)" :options="FieldKind.all.map(k => Opt(k, FieldKind.label(k)))" /></div>
          <label v-if="hasOptions(s)" class="field"><span>選択肢（1行に1つ）</span><textarea v-model="s.options"></textarea></label>
          <label class="small block"><input type="checkbox" v-model="s.required"> 必須（作成フォームで * を付ける）</label>
          <template v-if="Store.helpdesk">
            <label class="small block"><input type="checkbox" v-model="s.helpForm"> 窓口のフォームに出す（ユーザが新しい問い合わせで入力し、値を見られる）</label>
            <label v-if="s.helpForm" class="field"><span>入力のヒント（窓口のフォームで項目の下に出す）</span>
              <input type="text" v-model="s.helpHint" placeholder="例: 画面に出ているエラーの番号"></label>
          </template>
          <div class="actions"><button type="button" @click="s.editing := None">キャンセル</button><button type="submit" class="primary">保存</button></div>
        </form>
    </Modal>
  </section>
</template>

<style scoped>
.block { display: block; margin-top: .4rem; }
</style>
