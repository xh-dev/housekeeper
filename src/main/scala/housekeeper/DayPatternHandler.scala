package housekeeper

import me.xethh.utils.dateUtils.D
import me.xethh.utils.dateUtils.month.Month

import java.io.File
import java.util.Date
import java.util.function.Predicate
import java.util.regex.Pattern
import scala.util.{Failure, Try}
import housekeeper.Extensions._


type FileMatchTester = Predicate[String]
case class DayPatternHandler(_pattern: String) {
  assert(_pattern.contains("<year>"))
  assert(_pattern.contains("<month>"))
  assert(_pattern.contains("<day>"))

  def getDateFromFile(file: File): Try[Date] = {
    val matcher = pattern().matcher(file.getName)
    if (!matcher.matches()) {
      Failure(new RuntimeException("Fail to extract date from file"))
    } else {
      Option(matcher).tryTo { matcher =>
        val year = matcher.group("year").toInt
        val month = matcher.group("month").toInt.asTryWithMap(Month.ofCalendar)
          .throwIfFail()
          .get
        val day = matcher.group("day").toInt

        D.d().now().year(year).month(month).day(day).asDate()
      }
    }
  }

  def patternWithDate(date: Date): Try[Pattern] = {
    Option(date)
      .tryTo { d => D.d().from(d).view() }
      .map { dateViewer =>
        val newPatter = _pattern
          .replace("<year>", dateViewer.year().toString)
          .replace("<month>", dateViewer.month().toCalNumber.formatted("%02d"))
          .replace("<day>", dateViewer.day().formatted("%02d"))
        Pattern.compile(newPatter)
      }
  }

  def testerWithDate(date: Date): Try[FileMatchTester] = patternWithDate(date).map(_.asMatchPredicate())

  def pattern(): Pattern = {
    val patternStr = _pattern
      .replace("<year>", "(?<year>\\d{4})")
      .replace("<month>", "(?<month>\\d{2})")
      .replace("<day>", "(?<day>\\d{2})")
    Pattern.compile(patternStr)
  }

  def tester(): FileMatchTester = pattern().asMatchPredicate()

}
