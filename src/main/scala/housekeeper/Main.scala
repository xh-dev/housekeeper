package housekeeper

import me.xethh.utils.dateUtils.D
import me.xethh.utils.dateUtils.date.DateBuilder
import me.xethh.utils.dateUtils.month.Month
import me.xethh.utils.dateUtils.range.datetime.DatetimeRange

import java.io.{File, FilenameFilter}
import java.util.Date
import java.util.function.Predicate
import java.util.regex.Pattern
import scala.util.{Failure, Success, Try}
import housekeeper.DayPatternHandler
import housekeeper.ProcessLogicEnum.Handling
import org.slf4j.{Logger, LoggerFactory}

object Main {

  import housekeeper.Extensions._
  case class Config(
                     execute: Boolean = false,
                     source: File = null,
                     pattern: DayPatternHandler = null,
                     isDebug: Boolean = false,
                   )


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
      log.info("Keep:")
      result.getOrElse(Handling.Keep, Seq.empty).foreach(println)
      log.info("Remove:")
      result.getOrElse(Handling.Remove, Seq.empty).foreach(println)
      log.info("Skip:")
      result.getOrElse(Handling.Skipped, Seq.empty).foreach(println)
    }
    result
  }

  val log: Logger = LoggerFactory.getLogger(this.getClass)
  def main(args: Array[String]): Unit = {

    log.info("Start application")
    import scopt.OParser
    val builder = OParser.builder[Config]

    implicit val patternRead: scopt.Read[DayPatternHandler] = scopt.Read.reads(DayPatternHandler.apply)
    val parser1 = {
      import builder.*
      OParser.sequence(
        programName("housekeeper"),
        head("housekeeper", "0.0.2"),
        opt[Unit]("execute")
          .action((x, c) => c.copy(execute = true))
          .text("actually run the removing processing"),
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
      .map(_.get)
      .doSideEffect{it=>
        val msg = s"""
          |================= Config =================
          |Source:                ${config.source}
          |Pattern :              ${config.pattern._pattern}
          |Pattern regex:         ${config.pattern.pattern()}
          |Is debug:              ${config.isDebug}
          |Mode[execute|dry-run]: ${if (config.execute) "execute" else "dry-run"}
          |""".stripMargin
        log.info(msg)
      }
      .map { it =>
        it(Handling.Keep) match{
          case Some(files) =>
            log.info(s"[${files.size}] files to be kept")
          case None =>
            log.info("No file to be delete")
        }
        it(Handling.Skipped) match{
          case Some(files) =>
            log.info(s"[${files.size}] files to be skipped")
          case None =>
            log.info("No file to be delete")
        }
        it(Handling.Remove) match {
          case Some(files) =>
            log.info(s"[${files.size}] files to be deleted")
            files.foreach(file => log.info(s"To be deleted: ${file.getName}"))
            if (config.execute) {
              files.foreach(file => log.info(s"Deleting file[${file.getName}]: ${file.delete()}"))
              log.info("Actual action processed")
            } else{
              log.info("No file will be deleted actually")
            }
          case None =>
            log.info("No file to be delete")
        }
      }
      .throwIfFail()
  }
}
