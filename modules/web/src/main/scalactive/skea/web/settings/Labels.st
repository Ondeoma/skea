<script lang="scala">
import skea.web.*
import io.circe.syntax.*
import skea.shared.*


// ラベル
case class State(name: RV[String] = RV(""), color: RV[String] = RV("#2563eb"))

def initState() = State()

def labels = Store.meta.mapC(_.map(_.labels).getOrElse(Nil))

def save(l: LabelDto, name: String, color: String): Unit =
  Toasts.report(Api.putUnit(s"/api/projects/${Store.pid}/labels/${l.id}", LabelReq(name, Some(color)).asJson))(_ => Store.reloadMeta())

def add(s: State): Unit =
  Toasts.report(Api.post[LabelDto](s"/api/projects/${Store.pid}/labels", LabelReq(s.name.v, Some(s.color.v)).asJson)) { _ => s.name := ""; Store.reloadMeta() }

def remove(l: LabelDto): Unit =
  Dialog.confirm(s"ラベル「${l.name}」を削除しますか？（タスクからも外れます）") {
    Toasts.report(Api.delete(s"/api/projects/${Store.pid}/labels/${l.id}"))(_ => Store.reloadMeta())
  }
</script>

<template>
  <section>
    <h2>ラベル</h2>
    <div v-for="l in labels" :key="l.id" class="row lrow">
      <input type="color" :value="l.color" :disabled="Store.isProjectAdmin.mapC(!_)" @change="save(l, l.name, Dom.value(event))">
      <input type="text" :value="l.name" :disabled="Store.isProjectAdmin.mapC(!_)" @change="save(l, Dom.value(event), l.color)">
      <span class="label-chip" :style='"background:" + l.color'>{{ l.name }}</span>
      <button v-if="Store.isProjectAdmin" class="ghost small" @click="remove(l)">削除</button>
    </div>
    <p v-if="labels.mapC(_.isEmpty)" class="muted">ラベルはありません</p>
    <form class="row add" @submit.prevent="add(s)">
      <input type="color" v-model="s.color"><input type="text" v-model="s.name" placeholder="新しいラベル">
      <button type="submit" class="primary small">追加</button>
    </form>
  </section>
</template>

<style scoped>
.lrow { margin-bottom: .4rem; }
.add { margin-top: 1rem; }
input[type="color"] { width: 2.4rem; height: 2rem; padding: 2px; }
</style>
