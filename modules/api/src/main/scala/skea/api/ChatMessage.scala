package skea.api

import io.circe.{ACursor, Json}
import skea.shared.EventKind

/**
 * イベント（Webhook の本文と同じ JSON）を、チャット（stui）に投稿する文にする。
 * title は見出し（url があればリンクになる）、text は Markdown。
 */
object ChatMessage {

  final case class Message(title: String, text: String, url: String)

  /** task.updated の changes の field の表示名。 */
  private def fieldLabel(f: String): String = f match {
    case "title" => "タイトル"
    case "description" => "説明"
    case "assigneeId" => "担当者"
    case "reporterId" => "報告者"
    case "dueDate" => "期日"
    case "startDate" => "開始日"
    case "priority" => "優先度"
    case "type" => "種類"
    case "estimate" | "estimateMinutes" => "見積もり"
    case "points" => "ポイント"
    case "labels" => "ラベル"
    case "sprintId" => "スプリント"
    case "parentId" => "親タスク"
    case "epicId" => "エピック"
    case "fields" => "カスタムフィールド"
    case "links" => "リンク"
    case "dependencies" => "依存関係"
    case "attachments" => "添付ファイル"
    case "watchers" => "ウォッチ"
    case other => other
  }

  private def str(c: ACursor, k: String): String = c.get[String](k).getOrElse("")

  private def minutes(m: Int): String = {
    val (h, mm) = (m / 60, m % 60)
    if h == 0 then s"${mm}分" else if mm == 0 then s"${h}時間" else s"${h}時間${mm}分"
  }

  /** 引用（各行の先頭に > を付ける）。長ければ先を省く。 */
  private def quote(s: String, max: Int = 300): String = {
    val t = if s.length <= max then s else s.take(max) + "…"
    t.linesIterator.map("> " + _).mkString("\n")
  }

  def format(event: String, payload: Json, publicUrl: String): Message = {
    val c = payload.hcursor
    val d = c.downField("detail")
    val pKey = c.downField("project").get[String]("key").getOrElse("")
    val pName = c.downField("project").get[String]("name").getOrElse(pKey)
    val actor = c.downField("actor").get[String]("name").getOrElse("skea")
    val tKey = c.downField("task").get[String]("key").toOption
    val tTitle = c.downField("task").get[String]("title").getOrElse("")
    val base = publicUrl.stripSuffix("/")
    val board = if pKey.isEmpty then base else s"$base/p/$pKey/board"
    val taskUrl = tKey.fold(board)(k => s"$base/tasks/$k")
    val taskHead = tKey.fold(tTitle)(k => s"$k $tTitle")
    def task(icon: String, text: String) = Message(s"$icon $taskHead", text, taskUrl)
    def proj(icon: String, head: String, text: String) = Message(s"$icon [$pKey] $head", text, board)
    event match {
      case EventKind.TaskCreated => task("🆕", s"$actor がタスクを作成しました")
      case EventKind.TaskUpdated =>
        val fields = d.downField("changes").focus.flatMap(_.asArray).getOrElse(Vector.empty)
          .flatMap(_.hcursor.get[String]("field").toOption).map(fieldLabel).distinct
        task("✏️", s"$actor がタスクを更新しました" + (if fields.isEmpty then "" else fields.mkString("（", "・", "）")))
      case EventKind.TaskDeleted =>
        proj("🗑", s"${str(d, "key")} ${str(d, "title")}".trim, s"$actor がタスクを削除しました")
      case EventKind.StatusChanged =>
        task("🔄", s"$actor がステータスを「${str(d, "from")}」から「${str(d, "to")}」に変えました")
      case EventKind.CommentAdded => task("💬", s"$actor がコメントしました\n${quote(str(d, "text"))}")
      case EventKind.CommentUpdated => task("💬", s"$actor がコメントを編集しました")
      case EventKind.CommentDeleted => task("💬", s"$actor がコメントを削除しました")
      case EventKind.WorklogAdded =>
        val m = d.get[Int]("minutes").toOption.map(minutes).getOrElse("")
        task("⏱", s"$actor が作業時間を記録しました（$m${Option(str(d, "workDate")).filter(_.nonEmpty).fold("")("・" + _)}）")
      case EventKind.SprintCreated => proj("🏃", str(d, "name"), s"$actor がスプリントを作成しました")
      case EventKind.SprintStarted => proj("🏃", str(d, "name"), s"$actor がスプリントを開始しました")
      case EventKind.SprintCompleted => proj("🏁", str(d, "name"), s"$actor がスプリントを完了しました")
      case EventKind.CommitLinked =>
        val sha = str(d, "sha").take(7)
        val msg = str(d, "message").linesIterator.nextOption().getOrElse("")
        val repo = str(d, "repo")
        val link = Option(str(d, "url")).filter(_.nonEmpty).fold(s"`$sha`")(u => s"[`$sha`]($u)")
        task("🔗", s"コミット $link $msg" + (if repo.isEmpty then "" else s"（$repo）"))
      case EventKind.AttachmentAdded => task("📎", s"$actor がファイル「${str(d, "fileName")}」を添付しました")
      case "ping" => Message("skea からのテスト通知", s"$pName の通知の設定を確かめるためのテストです（$actor が送りました）。", board)
      case other => task("🔔", s"$actor: ${EventKind.label(other)}")
    }
  }
}
