package aoc.day

import zio.{Cause, Scope, Task, ZIO, ZIOAppArgs, ZIOAppDefault}
import zio.Console.printLine

import aoc.common.Tabulator
import aoc.common.Util.normalizeNewLine
import aoc.day.Day.dayString
import aoc.day.DayCase

trait Day[P1, P2] extends ZIOAppDefault:

  def logic: ZIO[Scope, Throwable, Unit] =
    ZIO.foreachDiscard(cases) { dayCase =>
      for
        _    <- printLine(s"${dayCase.style}----- ${dayCase.name} -----${Console.RESET}")
        in   <- dayCase.input.getInput.map(normalizeNewLine)
        s1   <- dayString("Part1", part1(in), dayCase.expectedOutputPart1)
        s2   <- dayString("Part2", part2(in), dayCase.expectedOutputPart2)
        table = Tabulator.formatTable(List(List("Part", "Answer", "Duration", "Verdict"), s1.toList, s2.toList))
        _    <- printLine(table)
      yield ()
    }

  def part1(in: String): Task[P1]

  def part2(in: String): Task[P2]

  def cases: List[DayCase[P1, P2]]

  override def run: ZIO[ZIOAppArgs & Scope, Any, Any] = logic
end Day

object Day:

  case class DayCaseSummary(part: String, answer: String, duration: String, verdict: Option[String]):
    def toList: List[String] = List(part, answer, duration, verdict.getOrElse(""))

  def dayString[P](part: String, zio: Task[P], maybeExpected: Option[P]): Task[DayCaseSummary] =
    zio.timed.either.flatMap {
      case Left(_: NotImplementedError) => ZIO.succeed(DayCaseSummary(part, "Not implemented", "", None))
      case Left(t)                      =>
        ZIO.logWarningCause(s"Error", Cause.fail(t)).as(DayCaseSummary(part, "Error", "", None))
      case Right((dur, answer))         =>
        val verdict: Option[String] = maybeExpected.map { expected =>
          if answer == expected then "CORRECT"
          else s"WRONG Expected $expected"
        }
        ZIO.succeed(
          DayCaseSummary(
            part,
            answer.toString,
            s"${dur.toMillis.toString} ms",
            verdict
          )
        )
    }

  implicit class TestIOOps[A](zio: Task[A]):

    def catchNotImplemented: ZIO[Any, Throwable, String] =
      zio.map(_.toString).catchSome { case _: NotImplementedError =>
        ZIO.succeed("Not implemented.")
      }
end Day
