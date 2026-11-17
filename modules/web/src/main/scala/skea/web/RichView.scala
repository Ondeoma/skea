package skea.web

import org.scalajs.dom
import skea.shared.*

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

/** 表示中の HTML の変化を見て、まとめて（次の描画の前に）f を呼ぶ. v-html で描き直されても付け直せるよう、body の下を見る. */
private object DomWatch {
  def onChange(delay: Option[Int])(f: () => Unit): Unit = {
    var queued = false
    val run = () => { queued = false; f() }
    new dom.MutationObserver((_, _) => if (!queued) {
      queued = true
      delay.fold(dom.window.requestAnimationFrame(_ => run()))(ms => dom.window.setTimeout(() => run(), ms))
    }).observe(dom.document.body, new dom.MutationObserverInit { childList = true; subtree = true })
    f()
  }
}

/**
 * サーバーが組み立てた HTML（.rich）のコードブロックのシンタックスハイライト.
 * 未処理のコードブロックだけに付ける（言語定義はそのとき読み込む）. ハイライトの計算は lowlight（window.skeaLowlight）.
 */
object Highlight {

  private def lowlight: js.Dynamic = dom.window.asInstanceOf[js.Dynamic].skeaLowlight

  private val Lang = "language-([A-Za-z0-9_+-]+)".r

  /** lowlight の結果（hast）を DOM にする. */
  private def toNode(n: js.Dynamic): dom.Node =
    if (n.`type`.toString == "text") dom.document.createTextNode(n.value.toString)
    else {
      val el = dom.document.createElement("span").asInstanceOf[dom.HTMLElement]
      val cls = if (js.isUndefined(n.properties) || n.properties == null) js.undefined else n.properties.className
      if (!js.isUndefined(cls) && cls != null)
        el.className = if (js.Array.isArray(cls)) cls.asInstanceOf[js.Array[String]].mkString(" ") else cls.toString
      if (!js.isUndefined(n.children)) n.children.asInstanceOf[js.Array[js.Dynamic]].foreach(c => el.appendChild(toNode(c)))
      el
    }

  private def highlightIn(root: dom.Document): Unit = {
    val codes = root.querySelectorAll(".rich pre code:not([data-hl])")
    if (codes.length > 0) {
      if (!lowlight.ready().asInstanceOf[Boolean]) lowlight.load().asInstanceOf[js.Promise[js.Any]].toFuture.foreach(_ => highlightIn(root))
      else codes.foreach { node =>
        val el = node.asInstanceOf[dom.HTMLElement]
        el.dataset("hl") = "1"
        val lang = Lang.findFirstMatchIn(el.className).map(_.group(1))
        lang.foreach(l => Option(el.parentElement).foreach(p => p.asInstanceOf[dom.HTMLElement].dataset("lang") = Editor.languageNames.getOrElse(l, l)))
        val text = el.textContent
        try {
          val tree = lowlight.highlight(lang.orUndefined, text)
          el.innerHTML = ""
          tree.children.asInstanceOf[js.Array[js.Dynamic]].foreach(c => el.appendChild(toNode(c)))
          el.classList.add("hljs")
        } catch { case _: Throwable => () } // ハイライトできないときはそのまま
      }
    }
  }

  /** 画面の起動時に1度呼ぶ. */
  def start(): Unit = DomWatch.onChange(None)(() => highlightIn(dom.document))

}

/**
 * 外部サービス（skiwi・skawau）へのリンクに、リンク先の名前を出す（docs/skiwi-integration.md・docs/skawau-integration.md）.
 * 表示中の HTML（説明・コメント・リンクの一覧）からサービスの URL のリンクを探し、まとめて名前を問い合わせる.
 * 編集中のエディタ（contenteditable）の中は書き換えない. 連携したサービスだけ動かす.
 */
object LinkTitles {

  private val Uuid = "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"

  // サービスごとの、URL の後ろの形（最後の () が問い合わせる ID）
  private val paths = Map(
    "skiwi"  -> s"/w/$Uuid/p/($Uuid)",          // ページ
    "skawau" -> s"/(?:view|office|files)/($Uuid)" // ファイル・フォルダ
  )

  private final class Watcher(provider: String, base: String, path: String) {
    private val re = js.RegExp("^" + base.replaceAll("""[.*+?^${}()|\[\]\\]""", """\\$0""") + path + "(?:[/?#]|$)")
    private var enabled = true
    private var timer: Option[Int] = None
    private val waiting = scala.collection.mutable.LinkedHashMap.empty[String, List[dom.HTMLAnchorElement]]

    // data-skiwi・data-skawau に状態を持つ
    private def state(a: dom.HTMLElement): Option[String] = a.dataset.get(provider)
    private def setState(a: dom.HTMLElement, v: String): Unit = a.dataset(provider) = v

    private def apply(a: dom.HTMLAnchorElement, item: Option[LinkTitleDto]): Unit = item match {
      case None => setState(a, "none")
      case Some(it) =>
        setState(a, "ok")
        a.classList.add("ext-link")
        a.title = s"$provider: ${it.title}"
        val label = s"${it.icon.filter(_.nonEmpty).getOrElse("📄")} ${it.title}"
        val text = Option(a.textContent).getOrElse("").trim
        // 文字が URL のままのリンクは名前に置き換え、文字を書いたリンクは名前を横に添える
        if (text.isEmpty || text == a.getAttribute("href") || text.startsWith(base)) a.textContent = label
        else if (a.nextElementSibling == null || !a.nextElementSibling.classList.contains("ext-title")) {
          val s = dom.document.createElement("span").asInstanceOf[dom.HTMLElement]
          s.className = "ext-title"
          s.textContent = label
          a.asInstanceOf[js.Dynamic].after(s)
        }
    }

    private def flush(): Unit = {
      timer = None
      val batch = waiting.take(100).toList
      batch.foreach((id, _) => waiting.remove(id))
      if (batch.nonEmpty) {
        Api.linkTitles(provider, batch.map(_._1)).onComplete { r =>
          r match {
            case scala.util.Success(res) if !res.connected =>
              enabled = false
              batch.foreach((_, as) => as.foreach(setState(_, "off")))
            case scala.util.Success(res) =>
              val byId = res.items.map(p => p.id.toString.toLowerCase -> p).toMap
              batch.foreach((id, as) => as.foreach(a => apply(a, byId.get(id))))
            case scala.util.Failure(_) =>
              // 問い合わせできなかったリンクは、次に描き直されたときにもう一度試す
              batch.foreach((_, as) => as.foreach(_.dataset.remove(provider)))
          }
          if (waiting.nonEmpty) timer = Some(dom.window.setTimeout(() => flush(), 50))
        }
      }
    }

    def scan(): Unit = if (enabled) {
      dom.document.querySelectorAll(s"a[href]:not([data-$provider])").foreach { n =>
        val a = n.asInstanceOf[dom.HTMLAnchorElement]
        if (a.closest("[contenteditable=\"true\"]") == null) {
          val m = re.exec(Option(a.getAttribute("href")).getOrElse(""))
          if (m != null) {
            setState(a, "pending")
            val id = m(1).get.toLowerCase
            waiting(id) = waiting.getOrElse(id, Nil) :+ a
          }
        }
      }
      if (waiting.nonEmpty && timer.isEmpty) timer = Some(dom.window.setTimeout(() => flush(), 100))
    }

    def reset(): Unit = {
      enabled = true
      dom.document.querySelectorAll(s"a[data-$provider=\"off\"]").foreach(a => a.asInstanceOf[dom.HTMLElement].dataset.remove(provider))
      scan()
    }

    DomWatch.onChange(Some(30))(() => scan())
  }

  private var watchers = Map.empty[String, Watcher]

  def start(svc: LinkServiceDto): Unit = if (svc.connected) watchers.get(svc.provider) match {
    case Some(w) => w.reset()
    case None    => paths.get(svc.provider).foreach(p => watchers += svc.provider -> new Watcher(svc.provider, svc.url, p))
  }

  /** 連携しているサービスを問い合わせて動かす. */
  def load(): Future[List[LinkServiceDto]] = {
    val f = Api.get[List[LinkServiceDto]]("/api/integrations/links")
    f.foreach(_.foreach(start))
    f
  }

}

/** 画像の加工（アイコンの登録用）. */
object Images {

  /** 画像を中央で正方形に切り取り、size px に縮める. GIF などで読めないときは元のファイルを返す. */
  def squareThumb(file: dom.File, size: Int = 256): Future[dom.File] = {
    val p = scala.concurrent.Promise[dom.File]()
    val url = dom.URL.createObjectURL(file)
    val img = dom.document.createElement("img").asInstanceOf[dom.HTMLImageElement]
    img.onload = (_: dom.Event) => {
      val s = math.min(img.naturalWidth, img.naturalHeight)
      val canvas = dom.document.createElement("canvas").asInstanceOf[dom.HTMLCanvasElement]
      canvas.width = math.min(size, s)
      canvas.height = canvas.width
      canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
        .drawImage(img, (img.naturalWidth - s) / 2.0, (img.naturalHeight - s) / 2.0, s, s, 0, 0, canvas.width, canvas.height)
      dom.URL.revokeObjectURL(url)
      canvas.asInstanceOf[js.Dynamic].toBlob((b: dom.Blob) =>
        p.success(if (b == null) file else new dom.File(js.Array(b), "avatar.png", new dom.FilePropertyBag { `type` = "image/png" })), "image/png")
    }
    img.addEventListener("error", (_: dom.Event) => { dom.URL.revokeObjectURL(url); p.success(file) })
    img.src = url
    p.future
  }

}
