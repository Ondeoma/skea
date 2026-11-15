package skea.shared

import io.circe.Json

// バックエンドと画面の両方で使う文字列の処理（同じ規則を2か所に書かないため、ここにまとめる）.

/** HTML のエスケープ. */
object Html {

  def escape(s: String): String = {
    val b = new StringBuilder(s.length + 16)
    s.foreach {
      case '<'  => b.append("&lt;")
      case '>'  => b.append("&gt;")
      case '&'  => b.append("&amp;")
      case '"'  => b.append("&quot;")
      case '\'' => b.append("&#39;")
      case c    => b.append(c)
    }
    b.toString
  }

  def unescape(s: String): String =
    s.replace("&lt;", "<").replace("&gt;", ">").replace("&quot;", "\"").replace("&#39;", "'").replace("&amp;", "&")

}

/** 分単位の時間の表示（90 → "1h 30m"）と入力（"1h 30m"・"1.5h"・"90m"・"90"・"2時間"）. */
object Minutes {

  def format(m: Int): String = {
    val h = m / 60
    val r = m % 60
    if (h > 0 && r > 0) s"${h}h ${r}m" else if (h > 0) s"${h}h" else s"${r}m"
  }

  /** 時間（1.5 など. 割り切れれば整数）. */
  def hours(m: Int): String = if (m % 60 == 0) (m / 60).toString else f"${m / 60.0}%.1f"

  private val HM = """^(?:(\d+(?:\.\d+)?)h)?(?:(\d+)m)?$""".r

  def parse(s: String): Option[Int] = {
    val t = s.trim.toLowerCase.replace("時間", "h").replace("分", "m").replace(" ", "")
    if (t.isEmpty) None
    else t.toIntOption.orElse(t match {
      case HM(h, m) if h != null || m != null =>
        Some(Option(h).flatMap(_.toDoubleOption).map(x => math.round(x * 60).toInt).getOrElse(0) + Option(m).flatMap(_.toIntOption).getOrElse(0))
      case _ => None
    }).filter(_ > 0)
  }

}

/** 変更履歴（task_event）の文言. 画面の通知・活動・履歴で使う. */
object EventText {

  private def str(d: Json, k: String): String = d.hcursor.get[String](k).getOrElse("")

  /** 変更したフィールド（task.updated の detail.changes）. */
  def changedFields(detail: Json): List[String] =
    detail.hcursor.downField("changes").as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.get[String]("field").toOption)

  /** 「SKEA-1 を作成しました」のような述部（操作した人の名前は呼び出し側で前に付ける）. taskKey がなければ「このタスク」. */
  def describe(kind: String, detail: Json, taskKey: Option[String]): String = {
    // タスクID の後ろだけ空白を入れる（「SKEA-1 を作成」「このタスクを作成」）
    val key = taskKey.getOrElse("このタスク")
    val subj = taskKey.fold("このタスク")(_ + " ")
    kind match {
      case EventKind.TaskCreated     => s"${subj}を作成しました"
      case EventKind.TaskDeleted     => s"${Option(str(detail, "key")).filter(_.nonEmpty).getOrElse(key)} を削除しました"
      case EventKind.StatusChanged   => s"${subj}を「${str(detail, "to")}」にしました"
      case EventKind.CommentAdded    => s"${subj}にコメントしました"
      case EventKind.CommentUpdated  => s"${subj}のコメントを編集しました"
      case EventKind.CommentDeleted  => s"${subj}のコメントを削除しました"
      case EventKind.WorklogAdded    =>
        s"${subj}に作業時間${detail.hcursor.get[Int]("minutes").toOption.fold("")(m => " " + Minutes.format(m))}を記録しました"
      case "worklog.updated"         => s"${subj}の作業時間を変更しました"
      case "worklog.deleted"         => s"${subj}の作業時間を削除しました"
      case EventKind.SprintCreated   => s"スプリント「${str(detail, "name")}」を作成しました"
      case EventKind.SprintStarted   => s"スプリント「${str(detail, "name")}」を開始しました"
      case EventKind.SprintCompleted => "スプリントを完了しました"
      case "sprint.updated"          => "スプリントを変更しました"
      case "sprint.deleted"          => "スプリントを削除しました"
      case "milestone.created"       => s"マイルストーン「${str(detail, "name")}」を追加しました"
      case "milestone.updated"       => s"マイルストーン「${str(detail, "name")}」を変更しました"
      case "milestone.deleted"       => "マイルストーンを削除しました"
      case EventKind.CommitLinked    => s"${subj}にコミット ${str(detail, "sha").take(8)} がリンクされました"
      case EventKind.AttachmentAdded => s"${subj}にファイル「${str(detail, "fileName")}」を添付しました"
      case "task.ranked"             => s"${subj}の並び順を変えました"
      case "comment.reacted"         => s"${subj}のコメントにリアクションしました"
      case EventKind.TaskUpdated     =>
        val names = changedFields(detail).map(TaskPatch.label).distinct
        if (names.isEmpty) s"${subj}を更新しました" else s"${subj}の${names.mkString("・")}を変更しました"
      case _ => s"${subj}を更新しました"
    }
  }

}
