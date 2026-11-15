<script lang="scala">
import skea.web.*
import skea.web.parts.*
import io.circe.syntax.*
import skea.shared.*


// ステータス（カンバンの列）。カテゴリ（作業前・進行中・待機中・完了）は集計・自動化・ガントの色に使う
case class State(name: RV[String] = RV(""), cat: RV[String] = RV(StatusCategory.Todo), color: RV[String] = RV("#64748b"))

def initState() = State()

def statuses = Store.meta.mapC(_.map(_.statuses).getOrElse(Nil))

def save(st: StatusDto, name: String, cat: String, color: String): Unit =
  Store.report(Api.putUnit(s"/api/projects/${Store.pid}/statuses/${st.id}", StatusReq(name, cat, Some(color)).asJson)) { _ => Store.reloadMeta(); Store.changed() }

def add(s: State): Unit =
  Store.report(Api.post[StatusDto](s"/api/projects/${Store.pid}/statuses", StatusReq(s.name.v, s.cat.v, Some(s.color.v)).asJson)) { _ => s.name := ""; Store.reloadMeta() }

def move(st: StatusDto, d: Int): Unit = {
  val ids = statuses.v.map(_.id)
  val i = ids.indexOf(st.id)
  val j = i + d
  if (j >= 0 && j < ids.size) {
    val re = ids.updated(i, ids(j)).updated(j, ids(i))
    Store.report(Api.putUnit(s"/api/projects/${Store.pid}/statuses/order", ReorderReq(re).asJson))(_ => Store.reloadMeta())
  }
}

def remove(st: StatusDto): Unit = {
  val others = statuses.v.filterNot(_.id == st.id)
  Dialog.choose(s"「${st.name}」を削除します。このステータスのタスクの移動先を選んでください。", others.map(o => o.id.toString -> o.name), "削除する", danger = true) { to =>
    others.find(_.id.toString == to).foreach { t =>
      Store.report(Api.delete(s"/api/projects/${Store.pid}/statuses/${st.id}?moveTo=${t.id}")) { _ => Store.reloadMeta(); Store.changed() }
    }
  }
}
val catOpts: List[Opt] = StatusCategory.all.map(c => Opt(c, StatusCategory.label(c)))
</script>

<template>
  <section>
    <h2>ステータス</h2>
    <p class="muted small">カンバンの列になります（上から左→右の順）。カテゴリが「完了」のステータスにしたタスクは完了扱いになります。</p>
    <div v-for="st in statuses" :key="st.id" class="row srow">
      <input type="color" :value="st.color" :disabled="Store.isProjectAdmin.mapC(!_)" @change="save(st, st.name, st.category, Dom.value(event))">
      <input type="text" :value="st.name" :disabled="Store.isProjectAdmin.mapC(!_)" @change="save(st, Dom.value(event), st.category, st.color)">
      <Combo :options="catOpts" :value="st.category" :small="true" :disabled="!Store.isProjectAdmin.v" :onChange="v => save(st, st.name, v, st.color)" />
      <template v-if="Store.isProjectAdmin">
        <button class="ghost small" @click="move(st, -1)">↑</button><button class="ghost small" @click="move(st, 1)">↓</button>
        <button class="ghost small" @click="remove(st)">削除</button>
      </template>
    </div>
    <form v-if="Store.isProjectAdmin" class="row add" @submit.prevent="add(s)">
      <input type="color" v-model="s.color">
      <input type="text" v-model="s.name" placeholder="新しいステータス">
      <Combo :bind="Some(s.cat)" :options="catOpts" />
      <button type="submit" class="primary small">追加</button>
    </form>
  </section>
</template>

<style scoped>
.srow { margin-bottom: .4rem; }
.add { margin-top: 1rem; }
input[type="color"] { width: 2.4rem; height: 2rem; padding: 2px; }
</style>
