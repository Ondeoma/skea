<script lang="scala">
import skea.web.*
import io.circe.Json
import org.scalajs.dom

import java.util.UUID

// リッチテキストエディタ（Tiptap）. onReady でエディタの操作（中身の取得など）を親に渡す.
case class Props(content: Option[Json] = None,
                 placeholder: String = "",
                 projectId: Option[UUID] = None,
                 taskId: Option[UUID] = None,
                 autofocus: Boolean = false,
                 onReady: EditorHandle => Unit = _ => (),
                 onSubmit: Unit => Unit = _ => (),
                 onChange: Json => Unit = _ => ())

// script はインスタンスで共有される object になるため、エディタはインスタンスごとの State に持つ
case class State(el: ElementRef[dom.HTMLElement] = ElementRef(), handle: RV[Option[EditorHandle]] = RV(None))

def initState() = State()

def mounted(s: State, props: Props): Unit =
  s.el.foreach { el =>
    val h = Editor.create(el, props.content, props.placeholder, props.projectId, props.taskId, () => props.onSubmit(()), props.autofocus, props.onChange)
    s.handle := Some(h)
    props.onReady(h)
  }

def unmounted(s: State): Unit = {
  s.handle.v.foreach(_.destroy())
  s.handle := None
}
</script>

<template>
  <div ref="s.el"></div>
</template>
