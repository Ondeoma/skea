package skea.web

import io.circe.Json
import io.circe.parser.parse
import org.scalajs.dom
import skea.shared.*

import java.util.UUID
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSGlobal

/** エディタ（frontend/src/editor.js. Tiptap を呼ぶ所だけ）. */
@js.native
@JSGlobal("skeaEditor")
private object EditorFacade extends js.Object {
  def create(area: dom.Element, opts: js.Object): TiptapHandle = js.native
}

/** editor.js の createEditor の戻り値. */
@js.native
private trait TiptapHandle extends js.Object {
  def getJSON(): js.Any = js.native
  def setJSON(j: js.Any): Unit = js.native
  def isEmpty(): Boolean = js.native
  def clear(): Unit = js.native
  def focus(): Unit = js.native
  def destroy(): Unit = js.native

  /** steps（[[コマンド名, ...引数], ...]）を1つの変更として続けて行う. */
  def run(steps: js.Array[js.Array[js.Any]]): Unit = js.native

  /** range（入力した「/…」）を消して steps を行う（文字のある行では次の行に入れる）. */
  def block(range: js.Any, steps: js.Array[js.Array[js.Any]]): Unit = js.native
  def editLink(): Unit = js.native
  def uploadFiles(files: js.Array[dom.File], pos: js.Any): Unit = js.native
  def isActive(tpe: String, attrs: js.Any): Boolean = js.native
  def attrs(tpe: String): js.Dynamic = js.native
  def isEditable(): Boolean = js.native
  def width(): Double = js.native
  def onUpdate(f: js.Function0[Unit]): Unit = js.native
}

/** 画面から使うエディタの操作（中身の取得・設定など）. */
final class EditorHandle private[web] (h: TiptapHandle, el: dom.Element) {
  def getJSON(): js.Any = h.getJSON()
  def setJSON(j: js.Any): Unit = h.setJSON(j)
  def isEmpty(): Boolean = h.isEmpty()
  def clear(): Unit = h.clear()
  def focus(): Unit = h.focus()
  def destroy(): Unit = { h.destroy(); el.innerHTML = ""; el.classList.remove("sk-editor") }
}

object Editor {

  def toJs(j: Json): js.Any = js.JSON.parse(j.noSpaces)

  def fromJs(a: js.Any): Json = parse(js.JSON.stringify(a)).getOrElse(Json.Null)

  /** コードブロックで選べる言語（id → 表示名）. lowlight の common と scala・dockerfile. 表示（.rich）の言語名にも使う. */
  val languageNames: Map[String, String] = Map(
    "plaintext" -> "プレーンテキスト",
    "bash" -> "Bash / シェル",
    "c" -> "C",
    "cpp" -> "C++",
    "csharp" -> "C#",
    "css" -> "CSS",
    "diff" -> "Diff",
    "go" -> "Go",
    "graphql" -> "GraphQL",
    "ini" -> "INI / TOML",
    "java" -> "Java",
    "javascript" -> "JavaScript",
    "json" -> "JSON",
    "kotlin" -> "Kotlin",
    "less" -> "Less",
    "lua" -> "Lua",
    "makefile" -> "Makefile",
    "markdown" -> "Markdown",
    "objectivec" -> "Objective-C",
    "perl" -> "Perl",
    "php" -> "PHP",
    "python" -> "Python",
    "r" -> "R",
    "ruby" -> "Ruby",
    "rust" -> "Rust",
    "scala" -> "Scala",
    "dockerfile" -> "Dockerfile",
    "scss" -> "SCSS",
    "shell" -> "シェルセッション",
    "sql" -> "SQL",
    "swift" -> "Swift",
    "typescript" -> "TypeScript",
    "vbnet" -> "VB.NET",
    "xml" -> "HTML / XML",
    "yaml" -> "YAML"
  )

  private def localeCompare(a: String, b: String): Int = a.asInstanceOf[js.Dynamic].localeCompare(b).asInstanceOf[Int]

  private lazy val languages: js.Array[js.Object] =
    languageNames.toList.sortWith((a, b) => localeCompare(a._2, b._2) < 0)
      .map((id, label) => js.Dynamic.literal(id = id, label = label).asInstanceOf[js.Object]).toJSArray

  /** セルの背景色（色名を data-bg に保存し、実際の色は CSS で決める. ダークモード対応）. */
  private val cellColors = List(
    "gray" -> "グレー",
    "brown" -> "ブラウン",
    "orange" -> "オレンジ",
    "yellow" -> "イエロー",
    "green" -> "グリーン",
    "blue" -> "ブルー",
    "purple" -> "パープル",
    "pink" -> "ピンク",
    "red" -> "レッド"
  )

  private def step(name: String, args: js.Any*): js.Array[js.Any] = ((name: js.Any) +: args).toJSArray
  private def steps(ss: js.Array[js.Any]*): js.Array[js.Array[js.Any]] = ss.toJSArray

  /** ツールバーのボタン（名前・表示（HTML）・説明・押したとき・押された状態か）. */
  private final case class Tool(label: String,
                                title: String,
                                run: TiptapHandle => Unit,
                                active: TiptapHandle => Boolean = _ => false)

  private def tool(label: String,
                   title: String,
                   cmd: String,
                   tpe: String,
                   attrs: js.UndefOr[js.Object] = js.undefined): Tool =
    Tool(label, title, h => h.run(steps(attrs.fold(step(cmd))(a => step(cmd, a)))), h => h.isActive(tpe, attrs.orNull))

  private def level(n: Int): js.Object = js.Dynamic.literal(level = n)
  private val table3x3 = js.Dynamic.literal(rows = 3, cols = 3, withHeaderRow = true)

  /** None は区切り. */
  private val tools: List[Option[Tool]] = List(
    Some(tool("<b>B</b>", "太字 (Ctrl+B)", "toggleBold", "bold")),
    Some(tool("<i>I</i>", "斜体 (Ctrl+I)", "toggleItalic", "italic")),
    Some(tool("<s>S</s>", "取り消し線", "toggleStrike", "strike")),
    Some(tool("&lt;/&gt;", "コード", "toggleCode", "code")),
    None,
    Some(tool("H2", "見出し", "toggleHeading", "heading", level(2))),
    Some(tool("H3", "小見出し", "toggleHeading", "heading", level(3))),
    Some(tool("•", "箇条書き", "toggleBulletList", "bulletList")),
    Some(tool("1.", "番号付きリスト", "toggleOrderedList", "orderedList")),
    Some(tool("☑", "ToDo", "toggleTaskList", "taskList")),
    Some(tool("❝", "引用", "toggleBlockquote", "blockquote")),
    Some(tool("{ }", "コードブロック", "toggleCodeBlock", "codeBlock")),
    None,
    Some(Tool("🔗", "リンク", _.editLink(), _.isActive("link", null))),
    Some(Tool("▦", "表", _.run(steps(step("insertTable", table3x3)))))
  )

  /** 「/」で開くメニューの項目. keys は検索に使う読み・英語. */
  private final case class Slash(icon: String, label: String, hint: String, keys: String, iconClass: String = "")(
    val run: (TiptapHandle, js.Any) => Unit)

  private def slashBlock(icon: String, label: String, hint: String, keys: String)(ss: js.Array[js.Any]*): Slash =
    Slash(icon, label, hint, keys)((h, range) => h.block(range, steps(ss*)))

  private def slashItems(upload: Boolean, pickFile: () => Unit): List[Slash] = List(
    slashBlock("H2", "見出し", "大きな見出し", "heading h2 midashi みだし")(step("setNode", "heading", level(2))),
    slashBlock("H3", "小見出し", "小さな見出し", "heading h3 komidashi こみだし")(step("setNode", "heading", level(3))),
    slashBlock("¶", "本文", "普通の段落", "paragraph text honbun ほんぶん")(step("setParagraph")),
    slashBlock("•", "箇条書き", "", "bullet list ul kajougaki かじょうがき")(step("toggleBulletList")),
    slashBlock("1.", "番号付きリスト", "", "ordered number list ol bangou ばんごう")(step("toggleOrderedList")),
    slashBlock("☑", "ToDo", "チェックボックス付きのリスト", "todo task check")(step("toggleTaskList")),
    slashBlock("❝", "引用", "", "quote blockquote inyou いんよう")(step("toggleBlockquote")),
    slashBlock("{ }", "コードブロック", "言語を選んでハイライト", "code block kodo こーど")(step("toggleCodeBlock")),
    slashBlock("▦", "表", "3×3 の表", "table hyou ひょう")(step("insertTable", table3x3)),
    slashBlock("―", "区切り線", "", "hr horizontal rule divider kugiri くぎり")(step("setHorizontalRule")),
    Slash("🔗", "リンク", "", "link url rinku りんく")((h, range) => {
      h.run(steps(step("deleteRange", range))); h.editLink()
    })
  ) ++
    Option.when(upload)(Slash("", "画像・ファイル", "添付して埋め込む", "image file gazou がぞう upload", "ico ico-image")((h, range) => {
      h.run(steps(step("deleteRange", range))); pickFile()
    })).toList ++ List(
      Slash("@", "メンション", "ユーザーに通知する", "mention user")((h, range) =>
        h.run(steps(step("deleteRange", range), step("insertContent", "@")))
      )
    )

  private def button(label: String, title: String, active: Boolean = false)(f: () => Unit): dom.HTMLButtonElement = {
    val b = dom.document.createElement("button").asInstanceOf[dom.HTMLButtonElement]
    b.`type` = "button"
    b.className = "sk-tb" + (if (active) " active" else "")
    b.innerHTML = label
    b.title = title
    b.addEventListener("mousedown", (e: dom.Event) => e.preventDefault())
    b.addEventListener("click", (_: dom.Event) => f())
    b
  }

  private def span(cls: String, text: String = ""): dom.HTMLElement = {
    val s = dom.document.createElement("span").asInstanceOf[dom.HTMLElement]
    s.className = cls
    s.textContent = text
    s
  }

  /**
   * 入力中の文字（@ や /）の下に出す候補の一覧（Tiptap の Suggestion の render）. 項目は {label, icon?, iconClass?, hint?}.
   */
  private def popup(emptyText: String): js.Function0[js.Object] = () => {
    var box: Option[dom.HTMLElement] = None
    var items = js.Array[js.Dynamic]()
    var selected = 0
    var command: js.Dynamic = null
    def str(v: js.Dynamic): String = if (js.isUndefined(v) || v == null) "" else v.toString
    def draw(): Unit = box.foreach { b =>
      b.innerHTML = ""
      if (items.isEmpty) b.appendChild(dom.document.createElement("div").asInstanceOf[dom.HTMLElement].tap(e => {
        e.className = "sk-mention-empty"; e.textContent = emptyText
      }))
      else items.zipWithIndex.foreach {
        (it, i) =>
          val btn = dom.document.createElement("button").asInstanceOf[dom.HTMLButtonElement]
          btn.`type` = "button"
          btn.className = "sk-mention-item" + (if (i == selected) " selected" else "")
          val (icon, iconClass) = (str(it.icon), str(it.iconClass))
          if (icon.nonEmpty || iconClass.nonEmpty) {
            val ic = span("sk-pop-icon")
            if (iconClass.nonEmpty) ic.appendChild(span(iconClass)) else ic.textContent = icon
            btn.appendChild(ic)
          }
          btn.appendChild(span("", str(it.label)))
          if (str(it.hint).nonEmpty) btn.appendChild(span("sk-pop-hint", str(it.hint)))
          btn.addEventListener(
            "mousedown",
            (ev: dom.Event) => { ev.preventDefault(); if (command != null) command(it) }
          )
          b.appendChild(btn)
          if (i == selected) btn.asInstanceOf[js.Dynamic].scrollIntoView(js.Dynamic.literal(block = "nearest"))
      }
    }
    def place(rect: js.Dynamic): Unit = box.foreach { b =>
      if (!js.isUndefined(rect) && rect != null) {
        val r = rect().asInstanceOf[dom.DOMRect]
        if (r != null && !js.isUndefined(r)) {
          b.style.left = s"${math.min(r.left + dom.window.scrollX, dom.window.scrollX + dom.window.innerWidth - 260)}px"
          b.style.top = s"${r.bottom + dom.window.scrollY + 4}px"
        }
      }
    }
    def take(props: js.Dynamic): Unit = {
      items = props.items.asInstanceOf[js.Array[js.Dynamic]]; selected = 0; command = props.command
      draw(); place(props.clientRect)
    }
    def close(): Unit = { box.foreach(_.remove()); box = None }
    js.Dynamic.literal(
      onStart = (
        (props: js.Dynamic) => {
          val b = dom.document.createElement("div").asInstanceOf[dom.HTMLElement]
          b.className = "sk-mention-box"
          dom.document.body.appendChild(b)
          box = Some(b)
          take(props)
        }): js.Function1[js.Dynamic, Unit],
      onUpdate = ((props: js.Dynamic) => take(props)): js.Function1[js.Dynamic, Unit],
      onKeyDown = (
        (p: js.Dynamic) => {
          val n = math.max(items.length, 1)
          p.event.asInstanceOf[dom.KeyboardEvent].key match {
            case "ArrowDown" => selected = (selected + 1) % n; draw(); true
            case "ArrowUp" => selected = (selected + items.length - 1) % n; draw(); true
            case "Enter" | "Tab" if selected < items.length => command(items(selected)); true
            case "Escape" => close(); true
            case _ => false
          }
        }): js.Function1[js.Dynamic, Boolean],
      onExit = (() => close()): js.Function0[Unit]
    )
  }

  // HTMLElement の hidden（scalajs-dom に無いので動的に設定する）
  private def hide(e: dom.HTMLElement, v: Boolean): Unit = e.asInstanceOf[js.Dynamic].hidden = v

  extension [A](a: A) {
    private def tap(f: A => Unit): A = { f(a); a }
  }

  /** 選んでいるものに応じたツールバー（画像の寄せ方向・大きさ、表のセルの色・行と列）. */
  private def contextBar(h: TiptapHandle, bar: dom.HTMLElement): () => Unit = () => {
    bar.innerHTML = ""
    def run(ss: js.Array[js.Any]*): () => Unit = () => h.run(steps(ss*))
    def label(t: String) = span("sk-ctx-label", t)
    def sep() = span("sk-sep")
    if (!h.isEditable()) hide(bar, true)
    else if (h.isActive("image", null)) {
      val align = Option(h.attrs("image").align).filterNot(js.isUndefined).map(_.toString)
      def setAlign(v: String | Null) =
        run(step("updateAttributes", "image", js.Dynamic.literal(align = v.asInstanceOf[js.Any])))
      def width(ratio: Option[Double]) = () => {
        val area = Option(h.width()).filter(_ > 0).getOrElse(600.0)
        val a = ratio.fold(js.Dynamic.literal(width = null, height = null))(r =>
          js.Dynamic.literal(width = math.round((area - 24) * r).toDouble, height = null)
        )
        h.run(steps(step("updateAttributes", "image", a)))
      }
      List(
        label("画像"),
        button("⇤", "左寄せ", align.forall(_ == "left"))(setAlign(null)),
        button("↔", "中央", align.contains("center"))(setAlign("center")),
        button("⇥", "右寄せ", align.contains("right"))(setAlign("right")),
        sep(),
        button("小", "幅 25%")(width(Some(0.25))),
        button("中", "幅 50%")(width(Some(0.5))),
        button("大", "幅 75%")(width(Some(0.75))),
        button("全幅", "幅 100%")(width(Some(1))),
        button("元", "元の大きさ")(width(None)),
        sep(),
        button("🗑", "画像を削除")(run(step("deleteSelection")))
      ).foreach(bar.appendChild)
      hide(bar, false)
    } else if (h.isActive("table", null)) {
      def bg(tpe: String) = Option(h.attrs(tpe).backgroundColor).filterNot(js.isUndefined).map(_.toString)
      val cur = bg("tableCell").orElse(bg("tableHeader"))
      def color(id: String | Null) = run(step("setCellAttribute", "backgroundColor", id.asInstanceOf[js.Any]))
      (List(label("セルの色"), button("""<span class="sk-swatch none"></span>""", "なし", cur.isEmpty)(color(null))) ++
        cellColors.map((id, name) =>
          button(s"""<span class="sk-swatch" data-bg="$id"></span>""", name, cur.contains(id))(color(id))
        ) ++
        List(
          sep(),
          label("行"),
          button("＋↑", "上に行を追加")(run(step("addRowBefore"))),
          button("＋↓", "下に行を追加")(run(step("addRowAfter"))),
          button("－", "行を削除")(run(step("deleteRow"))),
          sep(),
          label("列"),
          button("＋←", "左に列を追加")(run(step("addColumnBefore"))),
          button("＋→", "右に列を追加")(run(step("addColumnAfter"))),
          button("－", "列を削除")(run(step("deleteColumn"))),
          sep(),
          button("見出し行", "先頭の行を見出しにする／戻す")(run(step("toggleHeaderRow"))),
          button("🗑", "表を削除")(run(step("deleteTable")))
        )).foreach(bar.appendChild)
      hide(bar, false)
    } else hide(bar, true)
  }

  /**
   * エディタを作る. projectId を渡すと @メンションの候補をそのプロジェクトのメンバーから出す.
   * 画像・ファイルの貼り付け・ドロップは uploadUrl に送る（省略時は taskId のタスクの添付. ヘルプデスクの窓口では窓口の API の URL を渡す）.
   */
  def create(el: dom.Element,
             content: Option[Json],
             placeholder: String,
             projectId: Option[UUID],
             taskId: Option[UUID],
             onSubmit: () => Unit,
             autofocus: Boolean = false,
             onChange: Json => Unit = _ => (),
             uploadUrl: Option[String] = None): EditorHandle = {
    el.classList.add("sk-editor")
    def div(cls: String) = dom.document.createElement("div").asInstanceOf[dom.HTMLElement].tap(_.className = cls)
    val toolbar = div("sk-toolbar")
    val ctxBar = div("sk-toolbar sk-ctx")
    hide(ctxBar, true)
    val area = div("sk-area")
    el.appendChild(toolbar)
    el.appendChild(ctxBar)
    el.appendChild(area)
    val fileInput = dom.document.createElement("input").asInstanceOf[dom.HTMLInputElement]
    fileInput.`type` = "file"
    fileInput.multiple = true
    fileInput.style.display = "none"
    val pickFile = () => fileInput.click()

    // メンションの候補: 人と、まとめて届くグループ・役割（「グループ・3人」のように出す）
    val mentions: js.Function1[String, js.Promise[js.Array[js.Object]]] = (q: String) => {
      val ql = q.toLowerCase
      val people = projectId.fold(Future.successful(List.empty[UserBrief]))(Store.audience)
      val groups =
        projectId.fold(Future.successful(List.empty[MentionGroupDto]))(p => Store.mentionGroups(p).recover(_ => Nil))
      people.zip(groups).map {
        (us, gs) =>
          (us.filter(u => ql.isEmpty || u.name.toLowerCase.contains(ql)).map(u =>
            js.Dynamic.literal(id = u.id.toString, label = u.name)
          ) ++
            gs.filter(g => ql.isEmpty || g.name.toLowerCase.contains(ql)).map(g =>
              js.Dynamic.literal(
                id = g.id,
                label = g.name,
                icon = "👥",
                hint = s"${if (g.id.startsWith("r:")) "役割" else "グループ"}・${g.count}人"
              )
            ))
            .take(12).map(_.asInstanceOf[js.Object]).toJSArray
      }.toJSPromise
    }
    val uploadTo = uploadUrl.orElse(taskId.map(tid => s"/api/tasks/$tid/attachments"))
    val upload: js.Any = uploadTo match {
      case None => null
      case Some(url) =>
        val f: js.Function1[dom.File, js.Promise[js.Object]] = (file: dom.File) =>
          Api.sendFile[AttachmentDto](url, file).map(a =>
            js.Dynamic.literal(
              url = a.url,
              name = a.fileName,
              image = a.contentType.startsWith("image/")
            ).asInstanceOf[js.Object]
          ).toJSPromise
        f
    }
    var handle: TiptapHandle = null
    val slash = slashItems(uploadTo.isDefined, pickFile)
    val slashFn: js.Function1[String, js.Array[js.Object]] = (query: String) => {
      val q = query.toLowerCase
      slash.filter(i => q.isEmpty || i.label.toLowerCase.contains(q) || i.keys.contains(q)).take(12).map { i =>
        js.Dynamic.literal(
          icon = i.icon,
          label = i.label,
          hint = i.hint,
          iconClass = i.iconClass,
          run = ((range: js.Any) => i.run(handle, range)): js.Function1[js.Any, Unit]
        ).asInstanceOf[js.Object]
      }.toJSArray
    }
    val o = js.Dynamic.literal(
      content = content.map(toJs).getOrElse(null),
      placeholder = placeholder,
      languages = languages,
      cellColors = cellColors.map(_._1).toJSArray,
      mentions = mentions,
      slashItems = slashFn,
      popup = ((empty: String) => popup(empty)): js.Function1[String, js.Function0[js.Object]],
      upload = upload,
      autofocus = autofocus,
      prompt = (
        (m: String, d: String) => Dialog.promptJs(m, d)): js.Function2[String, String, js.Promise[js.Any]],
      alert = ((m: String) => Dialog.alert(m)): js.Function1[String, Unit],
      onSubmit = (() => onSubmit()): js.Function0[Unit],
      onChange = ((j: js.Any) => onChange(fromJs(j))): js.Function1[js.Any, Unit]
    )
    handle = EditorFacade.create(area, o.asInstanceOf[js.Object])
    val h = handle

    // ツールバー
    fileInput.addEventListener(
      "change",
      (_: dom.Event) => {
        val fs = fileInput.files
        h.uploadFiles((0 until fs.length).map(fs(_)).toJSArray, null); fileInput.value = ""
      }
    )
    toolbar.appendChild(fileInput)
    val buttons =
      (tools ++ Option.when(uploadTo.isDefined)(Some(Tool(
        """<span class="ico ico-image"></span>""",
        "画像・ファイルを添付",
        _ => pickFile()
      )))).flatMap {
        case None => toolbar.appendChild(span("sk-sep")); None
        case Some(t) => Some(button(t.label, t.title)(() => t.run(h)).tap(toolbar.appendChild) -> t)
      }
    toolbar.appendChild(span("sk-tb-hint", "「/」でメニュー"))
    val refresh = () =>
      buttons.foreach((b, t) => b.classList.toggle("active", t.active(h)))
    val ctx = contextBar(h, ctxBar)
    h.onUpdate(() => { refresh(); ctx() })
    ctx()
    new EditorHandle(h, el)
  }

}
