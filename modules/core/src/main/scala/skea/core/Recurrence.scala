package skea.core

import skea.shared.Frequency

import java.time.{DayOfWeek, LocalDate}

/** 繰り返しタスクの日付の計算。 */
object Recurrence {

  /** after より後で、最初に作る日。毎週は weekday（1=月〜7=日）、毎月は monthday（月末を超えるときは月末）。 */
  def next(
    frequency: String,
    weekday: Option[Int],
    monthday: Option[Int],
    after: LocalDate
  ): LocalDate = frequency match {
    case Frequency.Weekly =>
      val wd = DayOfWeek.of(weekday.filter(d => d >= 1 && d <= 7).getOrElse(1))
      Iterator.iterate(after.plusDays(1))(_.plusDays(1)).find(_.getDayOfWeek == wd).get
    case Frequency.Monthly =>
      val md = monthday.filter(d => d >= 1 && d <= 31).getOrElse(1)
      def in(
        m: LocalDate
      ) = m.withDayOfMonth(math.min(md, m.lengthOfMonth))
      val same = in(after.withDayOfMonth(1))
      if same.isAfter(after) then same else in(after.withDayOfMonth(1).plusMonths(1))
    case _ => after.plusDays(1)
  }
}
