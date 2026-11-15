package skea.core

import io.circe.Json
import io.circe.parser.parse

import skea.shared.Minutes

import java.time.LocalDate
import java.util.UUID

class CoreSpec extends munit.FunSuite:

  test("TaskKeys: コミットメッセージからタスクIDを取り出す") {
    assertEquals(TaskKeys.extract("SKEA-12 ログインを直す (fix SKEA-3, OTHER-1) xSKEA-9 SKEA-0"), List("SKEA-12", "SKEA-3", "OTHER-1"))
    assertEquals(TaskKeys.parse("skea-15"), Some(("SKEA", 15L)))
    assertEquals(TaskKeys.parse("SKEA"), None)
  }

  test("Minutes: 入力と表示") {
    assertEquals(Minutes.parse("1h 30m"), Some(90))
    assertEquals(Minutes.parse("1.5h"), Some(90))
    assertEquals(Minutes.parse("45"), Some(45))
    assertEquals(Minutes.parse("2時間"), Some(120))
    assertEquals(Minutes.parse("abc"), None)
    assertEquals(Minutes.format(90), "1h 30m")
  }

  test("FractionalIndex: 前後の間に入る") {
    val a = FractionalIndex.after(None)
    val b = FractionalIndex.after(Some(a))
    val m = FractionalIndex.between(Some(a), Some(b))
    assert(a < m && m < b, s"$a < $m < $b")
    val first = FractionalIndex.between(None, Some(a))
    assert(first < a)
    // 逆順・同じ値でも例外にしない（後ろに置く）
    assert(FractionalIndex.between(Some(b), Some(a)) > b)
  }

  test("SearchQuery: 絞り込み構文") {
    val q = SearchQuery.parse("""project:SKEA status:進行中 -label:保留 due<2026-12-01 "ログイン 画面" バグ -除外 order:-updated,key""").toOption.get
    assertEquals(q.filters.map(f => (f.key, f.op, f.value, f.negate)), List(
      ("project", SearchQuery.Op.Eq, "SKEA", false),
      ("status", SearchQuery.Op.Eq, "進行中", false),
      ("label", SearchQuery.Op.Eq, "保留", true),
      ("due", SearchQuery.Op.Lt, "2026-12-01", false)
    ))
    assertEquals(q.text, List("ログイン 画面", "バグ"))
    assertEquals(q.negText, List("除外"))
    assertEquals(q.order, List("updated" -> false, "key" -> true))
    assert(SearchQuery.parse("order:foo").isLeft)
    val today = LocalDate.of(2026, 11, 15)
    assertEquals(SearchQuery.date("-7d", today), Some(LocalDate.of(2026, 11, 8)))
    assertEquals(SearchQuery.date("today", today), Some(today))
    assertEquals(SearchQuery.phrase("a\"b"), "\"a\\\"b\"")
  }

  test("RichText: 許可した要素だけを HTML にする") {
    val id = UUID.randomUUID()
    val doc = parse(s"""{"type":"doc","content":[
      {"type":"paragraph","content":[{"type":"text","text":"<b>x</b>","marks":[{"type":"bold"}]},
        {"type":"text","text":"link","marks":[{"type":"link","attrs":{"href":"javascript:alert(1)"}}]},
        {"type":"mention","attrs":{"id":"$id","label":"山田"}}]},
      {"type":"script","content":[{"type":"text","text":"y"}]},
      {"type":"image","attrs":{"src":"javascript:x"}}
    ]}""").toOption.get
    val html = RichText.toHtml(doc)
    assertEquals(html, s"""<p><strong>&lt;b&gt;x&lt;/b&gt;</strong>link<span class="mention" data-user-id="$id">@山田</span></p>y""")
    assertEquals(RichText.mentions(doc), Set(id))
    assertEquals(RichText.toPlain(doc), "<b>x</b>link@山田\ny")
  }

  test("RichText: 画像の幅・寄せ方向と、セルの色（許可した値だけ）") {
    val doc = parse("""{"type":"doc","content":[
      {"type":"image","attrs":{"src":"/api/attachments/x","alt":"a","width":320.4,"align":"center"}},
      {"type":"image","attrs":{"src":"/api/attachments/y","width":"100%","align":"evil\" onload=\"x"}},
      {"type":"table","content":[{"type":"tableRow","content":[
        {"type":"tableCell","attrs":{"backgroundColor":"yellow"},"content":[{"type":"paragraph"}]},
        {"type":"tableCell","attrs":{"backgroundColor":"url(x)"},"content":[{"type":"paragraph"}]}]}]}
    ]}""").toOption.get
    assertEquals(RichText.toHtml(doc),
      """<img src="/api/attachments/x" alt="a" width="320" data-align="center" loading="lazy"><img src="/api/attachments/y" alt="" loading="lazy">""" +
        """<table><tbody><tr><td data-bg="yellow"><p></p></td><td><p></p></td></tr></tbody></table>""")
  }

  test("RichText: グループ・役割へのメンション") {
    val g = UUID.randomUUID()
    val doc = parse(s"""{"type":"doc","content":[{"type":"paragraph","content":[
      {"type":"mention","attrs":{"id":"g:$g","label":"開発"}},{"type":"mention","attrs":{"id":"r:all","label":"全員"}},
      {"type":"mention","attrs":{"id":"r:evil","label":"x"}}]}]}""").toOption.get
    assertEquals(RichText.groupMentions(doc), Set(s"g:$g", "r:all"))
    assertEquals(RichText.mentions(doc), Set.empty[UUID])
    assert(RichText.toHtml(doc).contains(s"""<span class="mention group" data-group="g:$g">@開発</span>"""))
  }

  test("Recurrence: 毎日・毎週・毎月（月末に丸める）") {
    import skea.shared.Frequency
    val d = LocalDate.of(2026, 11, 15) // 日曜
    assertEquals(Recurrence.next(Frequency.Daily, None, None, d), LocalDate.of(2026, 11, 16))
    assertEquals(Recurrence.next(Frequency.Weekly, Some(1), None, d), LocalDate.of(2026, 11, 16))
    assertEquals(Recurrence.next(Frequency.Weekly, Some(7), None, d), LocalDate.of(2026, 11, 22))
    assertEquals(Recurrence.next(Frequency.Monthly, None, Some(20), d), LocalDate.of(2026, 11, 20))
    assertEquals(Recurrence.next(Frequency.Monthly, None, Some(15), d), LocalDate.of(2026, 12, 15))
    assertEquals(Recurrence.next(Frequency.Monthly, None, Some(31), LocalDate.of(2027, 1, 31)), LocalDate.of(2027, 2, 28))
  }

  test("RichText: Markdown 風の入力") {
    val doc = RichText.fromMarkdown("# 見出し\n\n- a\n- **b**\n\n本文 [リンク](https://example.com)")
    assertEquals(RichText.toHtml(doc),
      """<h1>見出し</h1><ul><li><p>a</p></li><li><p><strong>b</strong></p></li></ul><p>本文 <a href="https://example.com" target="_blank" rel="noopener noreferrer nofollow">リンク</a></p>""")
    assert(RichText.isBlank(RichText.fromPlain("  ")))
  }

  test("Automation: 条件とスマート値") {
    val ctx = parse("""{"task":{"key":"SKEA-1","status":{"name":"完了","category":"done"},"labels":["バグ","UI"],"spentMinutes":30,
      "assignee":null,"dueDate":"2026-11-20"},"event":{"changes":["statusId"]}}""").toOption.get
    val today = LocalDate.of(2026, 11, 15)
    def c(s: String) = Automation.parseCondition(parse(s).toOption.get).toOption.get
    assert(Automation.eval(c("""{"field":"task.status.category","op":"eq","value":"done"}"""), ctx, today))
    assert(Automation.eval(c("""{"field":"task.labels","op":"contains","value":"バグ"}"""), ctx, today))
    assert(Automation.eval(c("""{"field":"task.assignee","op":"empty"}"""), ctx, today))
    assert(Automation.eval(c("""{"field":"task.spentMinutes","op":"gt","value":"20"}"""), ctx, today))
    assert(Automation.eval(c("""{"field":"task.dueDate","op":"lt","value":"{{today+7}}"}"""), ctx, today))
    assert(Automation.eval(c("""{"all":[{"field":"task.statusId","op":"changed"},{"not":{"field":"task.labels","op":"in","value":"保留,後回し"}}]}"""), ctx, today))
    assert(!Automation.eval(c("""{"any":[{"field":"task.key","op":"matches","value":"^OTHER"}]}"""), ctx, today))
    assertEquals(Automation.render("{{task.key}}: {{task.assignee.name | default:未割り当て}} / {{task.labels}} / {{today+1}}", ctx, today),
      "SKEA-1: 未割り当て / バグ, UI / 2026-11-16")
  }

  test("Automation: ルールの読み込み") {
    val trig = parse("""{"type":"status_changed","to":["完了"],"condition":{"field":"task.labels","op":"contains","value":"バグ"}}""").toOption.get
    val acts = parse("""[{"type":"comment","text":"完了: {{task.key}}"},
      {"type":"if","condition":{"field":"webResponse.status","op":"eq","value":200},"then":[{"type":"log","message":"ok"}],"else":[]},
      {"type":"web_request","url":"https://example.com/hook","body":{"key":"{{task.key}}"}}]""").toOption.get
    assert(Automation.validate(trig, acts).isRight)
    assert(Automation.validate(Json.obj("type" -> Json.fromString("nope")), acts).isLeft)
    assert(Automation.validate(trig, Json.arr()).isLeft)
  }

  test("Access: 実効権限") {
    assertEquals(Access.effective(isAdmin = false, Some(Role.View), List(Role.Edit)), Some(Role.Edit))
    assertEquals(Access.effective(isAdmin = true, None, Nil), Some(Role.Admin))
    assertEquals(Access.effective(isAdmin = false, None, Nil), None)
  }
