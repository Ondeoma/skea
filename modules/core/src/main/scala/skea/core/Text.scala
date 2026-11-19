package skea.core

/** タスクID（SKEA-123）。 */
object TaskKeys {
  private val KeyRe = """(?<![A-Za-z0-9_\-])([A-Z][A-Z0-9]{1,9})-([1-9][0-9]{0,9})(?![A-Za-z0-9_])""".r

  /** 文章（コミットメッセージなど）に含まれるタスクID（重複なし、出てきた順）。 */
  def extract(text: String): List[String] =
    KeyRe.findAllMatchIn(text).map(m => s"${m.group(1)}-${m.group(2)}").toList.distinct

  /** "SKEA-12" を (SKEA, 12) に分ける。 */
  def parse(key: String): Option[(String, Long)] =
    key.trim.toUpperCase match {
      case s"$p-$n" if ProjectKey.valid(p) => n.toLongOption.filter(_ > 0).map(p -> _)
      case _ => None
    }
}
