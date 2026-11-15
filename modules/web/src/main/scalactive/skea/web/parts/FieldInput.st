<script lang="scala">
import skea.web.*
import io.circe.Json
import io.circe.syntax.*
import skea.shared.*

// カスタムフィールドの入力欄（種類ごと）. 値が変わると onChange に新しい値（空なら null）を渡す.
// live なら入力のたび（作成フォーム）、そうでなければ確定したとき（タスク画面）に知らせる.
case class Props(field: CustomFieldDto, value: Option[Json], onChange: Json => Unit, disabled: Boolean = false, live: Boolean = false)

/** チェックボックスの選択（押すたびに描き直されなくても正しく足し引きできるよう、状態に持つ）. */
case class State(checks: RV[List[String]] = RV(Nil))

def initState(props: Props) = State(RV(props.value.flatMap(_.as[List[String]].toOption).getOrElse(Nil)))

def str(props: Props): String = props.value.flatMap(_.asString).getOrElse("")
def opt(v: String): Json = if (v.isEmpty) Json.Null else Json.fromString(v)

def text(props: Props, v: String, input: Boolean): Unit = if (input == props.live) props.onChange(opt(v))

def toggle(s: State, props: Props, o: String, on: Boolean): Unit = {
  s.checks := (if (on) (s.checks.v :+ o).distinct else s.checks.v.filterNot(_ == o))
  props.onChange(if (s.checks.v.isEmpty) Json.Null else s.checks.v.asJson)
}

def isChecked(s: State, o: String) = s.checks.mapC(_.contains(o))
def kind(props: Props): String = props.field.kind
</script>

<template>
  <input v-if='kind(props) == "text"' type="text" :value="str(props)" :disabled="props.disabled"
    @input="text(props, Dom.value(event), true)" @change="text(props, Dom.value(event), false)">
  <textarea v-else-if='kind(props) == "textarea"' :disabled="props.disabled" v-html="Html.escape(str(props))"
    @input="text(props, Dom.value(event), true)" @change="text(props, Dom.value(event), false)"></textarea>
  <Combo v-else-if='kind(props) == "select"' :options="Opt.names(props.field.options)" :value="str(props)" clearLabel="（未選択）"
    :disabled="props.disabled" :onChange="v => props.onChange(opt(v))" />
  <div v-else-if='kind(props) == "radio"' class="chips">
    <label v-for="o in props.field.options" class="chip"><input type="radio" :name='"cf-" + props.field.id' :checked="str(props) == o"
      :disabled="props.disabled" @change="props.onChange(Json.fromString(o))"> {{ o }}</label>
  </div>
  <div v-else class="chips">
    <label v-for="o in props.field.options" class="chip"><input type="checkbox" :checked="isChecked(s, o)" :disabled="props.disabled"
      @change="toggle(s, props, o, Dom.checked(event))"> {{ o }}</label>
  </div>
</template>
