package housekeeper

import housekeeper.Main.Handling
import me.xethh.utils.dateUtils.D
import me.xethh.utils.dateUtils.date.DateBuilder
import me.xethh.utils.dateUtils.month.Month
import me.xethh.utils.dateUtils.range.datetime.DatetimeRange

import java.io.{File, FilenameFilter}
import java.util.Date
import java.util.function.Predicate
import java.util.regex.Pattern
import scala.util.{Failure, Success, Try}

object Main {

  case class ExtendedAny[A](a: A) {
    def nothingHappen(operation: A => Unit): A = {
      operation(a)
      a
    }

    def asTry(): Try[A] = Success(a)

    def asTryWithMap[B](operation: A => B): Try[B] = asTry().map(operation)
  }

  case class ExtendedOption[V](option: Option[V]) {
    def ifUnexpected(msg: String): Try[V] = {
      option match {
        case Some(v) => Success(v)
        case None => Failure(new RuntimeException(msg))
      }
    }

    def allAsExpected[A](indexedSeq: Seq[Try[A]], msg: String): Try[Seq[A]] = {
      if (indexedSeq.exists(_.isFailure)) Failure(new RuntimeException(msg)) else Success(indexedSeq.map(_.get))
    }

    def tryTo[A](operation: V => A): Try[A] = ifUnexpected("Passing empty to operation").map(v => operation(v))

  }

  case class ExtendedGroupTry[V, A <: Seq[Try[V]]](tries: A) {
    def testAllSuccess(msg: String | Exception): Try[Seq[V]] = if (tries.exists(_.isFailure)) Failure(msg match {
      case msg: String => new RuntimeException(msg)
      case ex: Exception => ex
    }) else Success(tries.map(_.get))
  }

  case class ExtendedTry[V](t: Try[V]) {
    def throwIfFail(): Try[V] = {
      if (t.isFailure) {
        throw t.failed.get
      } else {
        t
      }
    }
  }

  //case class ExtendedMap[K, V](m: Map[K, Array[V]]) {
  //  def groupByInternalKey(key: K)(operation: V => K): Map[K, Array[V]] = {
  //    m.get(key) match {
  //      case Some(values) =>
  //        var mm = m.removed(key)
  //        val newM = m(key).groupBy[K](operation)
  //        for ((key, value) <- newM) {
  //          val newValue:Array[V] =  mm.get(key) match {
  //            case Some(vv) => vv.toList
  //            case None => value
  //          }
  //          mm = mm + (key-> newValue)
  //        }
  //        mm
  //      case None =>
  //        m
  //    }
  //  }
  //}

  implicit def implicitToExtendedOption[V](option: Option[V]): ExtendedOption[V] = ExtendedOption(option)

  implicit def implicitToExtendedGroupTry[V](indexedSeq: Seq[Try[V]]): ExtendedGroupTry[V, Seq[Try[V]]] = ExtendedGroupTry(indexedSeq)

  implicit def implicitToExtendedTry[V](t: Try[V]): ExtendedTry[V] = ExtendedTry(t)

  implicit def implicitToExtendedAny[V](t: V): ExtendedAny[V] = ExtendedAny(t)

  //implicit def implicitToExtendedMap[K, V](m: Map[K, Array[V]]): ExtendedMap[K, V] = ExtendedMap(m)

  //implicit def implicitToAnyOption[A](option: A): Option[A] = Option.apply(option)
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


  case class Config(
                     execute: Boolean = false,
                     isFile: Boolean = true,
                     source: File = null,
                     pattern: DayPatternHandler = null,
                     isDebug: Boolean = false,
                   )

  enum Handling:
    case Keep, Remove, Skipped

  def process(config: Config): Map[Handling, Seq[File]] = {
    val files = config.source.listFiles(new FilenameFilter {
      val filenameTester: FileMatchTester = config.pattern.tester()

      override def accept(dir: File, name: String): Boolean = filenameTester.test(name)
    })

    val today = D.d().now()

    val maxRange = today.rangeFrom(today.addYear(-10).month(Month.JAN).firstDayOfMonth())

    def checkInRangeAndAddToList(range: DatetimeRange, _processing: DateBuilder, start: Date, extra: DateBuilder => DateBuilder = d => d): List[Date] = {
      var processing = _processing
      var continue = true
      var list = List.empty[Date]
      while (continue) {
        if (range.dateInRange(processing.asDate())) {
          list = processing.asDate() :: list
        }
        processing = processing.addMonths(-1)
        processing = extra(processing)

        if (processing.before(start)) continue = false
      }
      list
    }

    val ranges = List(
      (today.addDays(-7), today, (0 to 7).map(d => today.addDays(-1 * d).asDate()).toList),
      (today.addDays(-8), today.addMonths(-6).firstDayOfMonth()).asTryWithMap { xxx =>
        val (end, start) = xxx
        val endView = end.view()
        val range = D.d().rangeOn(start).editEnd().year(endView.year()).month(endView.month()).day(endView.day()).back()
        var list = List.empty[Date]
        var processing = today.endDayOfMonth()

        processing = today.endDayOfMonth()
        list = checkInRangeAndAddToList(range, processing, start.asDate()) ::: list

        processing = today.day(15)
        list = checkInRangeAndAddToList(range, processing, start.asDate()) ::: list

        processing = today.firstDayOfMonth()
        list = checkInRangeAndAddToList(range, processing, start.asDate()) ::: list
        (start, end, list)
      }.throwIfFail().get,
      (today.addMonths(-7).endDayOfMonth(), today.addYear(-10).month(Month.JAN).firstDayOfMonth()).asTryWithMap { xxx =>
        val (end, start) = xxx
        val endView = end.view()
        val range = D.d().rangeOn(start).editEnd().year(endView.year()).month(endView.month()).day(endView.day()).back()
        var list = List.empty[Date]
        var processing = today.endDayOfMonth()
        var continue = true

        processing = today.endDayOfMonth()
        list = checkInRangeAndAddToList(range, processing, start.asDate(), extra = processing => processing.endDayOfMonth()) ::: list

        (start, end, list)
      }.throwIfFail().get
    )
      .flatMap(_._3)

    val result = files.map { f =>
      config.pattern.getDateFromFile(f).map((f, _))
    }.toList
      .testAllSuccess("Fail extract date from file")
      .throwIfFail()
      .get
      .groupBy { t =>
        val (file, date) = t
        if (maxRange.dateInRange(date)) {
          if (ranges.contains(date)) {
            Handling.Keep
          } else {
            Handling.Remove
          }
        } else {
          Handling.Skipped
        }
      }
      .map { f => f._1 -> f._2.map(_._1) }

    if (config.isDebug) {
      println("Keep:")
      result.getOrElse(Handling.Keep, Seq.empty).foreach(println)
      println("Remove:")
      result.getOrElse(Handling.Remove, Seq.empty).foreach(println)
      println("Skip:")
      result.getOrElse(Handling.Skipped, Seq.empty).foreach(println)
    }
    result
  }

  def main(args: Array[String]): Unit = {
    import scopt.OParser
    val builder = OParser.builder[Config]

    implicit val patternRead: scopt.Read[DayPatternHandler] = scopt.Read.reads(DayPatternHandler.apply)
    val parser1 = {
      import builder.*
      OParser.sequence(
        programName("housekeeper"),
        head("housekeeper", "0.0.1"),
        opt[Boolean]("execute")
          .action((x, c) => c.copy(execute = x))
          .text("actually run the removing processing"),
        opt[Boolean]("is-file")
          .action((x, c) => {
            c.copy(isFile = x)
          })
          .text("test if the housekeeping task is a file, default is true"),
        opt[Boolean]("debug")
          .action((x, c) => c.copy(isDebug = x))
          .text("activate debugging log"),
        opt[DayPatternHandler]("pattern")
          .required()
          .action((x, c) => c.copy(pattern = x))
          .text("Pattern to match"),
        arg[File]("source")
          .required()
          .validate(f => if (f.exists()) success else failure("source file does not exists"))
          .action((x, c) => {
            c.copy(source = x)
          })
          .text("The source directory or source file")
      )
    }

    val config = OParser.parse(parser1, args, Config()).ifUnexpected("Fail parse exception")
      .throwIfFail().get

    config.asTryWithMap(process)
      .throwIfFail()
      .map { it =>
        it.get(Handling.Remove) match {
          case Some(files) =>
            println(s"[${files.size}] files to be deleted")
            files.foreach(file => println(s"To be deleted: ${file.getName}"))
            if (config.execute) {
              files.foreach(file => println(s"Deleting file[${file.getName}]: ${file.delete()}"))
              println("Actual action processed")
            }
          case None =>
            println("No file to be delete")
        }
      }
      .throwIfFail()
  }
}
