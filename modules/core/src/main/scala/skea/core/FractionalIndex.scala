package skea.core

/** カンバン・バックログの並び順用 fractional index（skiwi と同じ）。
  * 文字列のバイト順（DB では collate "C"）で比較する。キーは末尾に '0' を持たない。
  * rocicorp/fractional-indexing の midpoint アルゴリズムの移植。
  */
object FractionalIndex:
  private val digits = (('0' to '9') ++ ('A' to 'Z') ++ ('a' to 'z')).mkString
  private val zero   = digits.head
  private def idx(c: Char): Int = digits.indexOf(c)

  /** lo < result < hi（lo, hi が None の場合はそれぞれ下限・上限なし）。lo >= hi のときは lo の後ろに置く。 */
  def between(lo: Option[String], hi: Option[String]): String =
    val a = lo.getOrElse("")
    val b = hi.filter(x => a < x)
    midpoint(a.reverse.dropWhile(_ == zero).reverse, b.map(x => x.reverse.dropWhile(_ == zero).reverse).filter(_.nonEmpty))

  private def midpoint(a: String, b: Option[String]): String =
    b match
      case Some(bb) =>
        var n = 0
        while n < bb.length && (if n < a.length then a(n) else zero) == bb(n) do n += 1
        if n > 0 then return bb.take(n) + midpoint(a.drop(n), Some(bb.drop(n)))
      case None => ()
    val digitA = if a.nonEmpty then idx(a(0)) else 0
    val digitB = b.map(x => idx(x(0))).getOrElse(digits.length)
    if digitB - digitA > 1 then digits(math.round(0.5 * (digitA + digitB)).toInt).toString
    else if b.exists(_.length > 1) then b.get.take(1)
    else digits(digitA).toString + midpoint(a.drop(1), None)

  /** 末尾に追加するときの値。 */
  def after(last: Option[String]): String = between(last, None)
