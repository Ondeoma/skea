<script lang="scala">
import skea.web.*
import skea.web.parts.*
import io.circe.syntax.*
import skea.shared.*

import java.util.UUID

// 一般: 名前・説明・リーダー
case class State(name: RV[String] = RV(""), desc: RV[String] = RV(""), lead: RV[String] = RV(""))

def initState() = Store.meta.v.fold(State())(m =>
  State(RV(m.project.name), RV(m.project.description), RV(m.project.lead.map(_.id.toString).getOrElse(""))))

def pid = Store.meta.v.map(_.project.id)

def save(s: State): Unit = pid.foreach { p =>
  Toasts.report(Api.patchUnit(s"/api/projects/$p",
    UpdateProjectReq(Some(s.name.v), Some(s.desc.v), Option(s.lead.v).filter(_.nonEmpty).map(UUID.fromString), None).asJson)) { _ =>
    Toasts.show("保存しました")
    Store.loadProjects()
    Store.reloadMeta()
  }
}

def members = Store.meta.mapC(_.map(_.members.map(_.user)).getOrElse(Nil))
def key = Store.meta.mapC(_.map(_.project.key).getOrElse(""))
</script>

<template>
  <section>
    <h2>一般</h2>
    <label class="field"><span>キー（変更できません）</span><input type="text" :value="key" disabled></label>
    <label class="field"><span>名前</span><input type="text" v-model="s.name"></label>
    <label class="field"><span>説明</span><textarea v-model="s.desc"></textarea></label>
    <div class="field"><span>リーダー</span><Combo :bind="Some(s.lead)" :live="Some(members.mapC(Opts.users))" clearLabel="なし" /></div>
    <button class="primary" @click="save(s)">保存</button>
  </section>
</template>
