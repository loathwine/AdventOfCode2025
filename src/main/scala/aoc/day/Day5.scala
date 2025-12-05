package aoc.day

import zio.*

import aoc.common.{ImplicitParser, Parser}
import aoc.common.ImplicitParser.{*, given}
import aoc.common.Input.{InputString, ResourceInput}
import aoc.day.DayCase.{Puzzle, Test}

object Day5 extends Day:
  type P1 = Int
  type P2 = BigInt
  type Id = BigInt

  case class Range(from: BigInt, to: BigInt):
    def contains(id: Id): Boolean = from <= id && id <= to
    def size: BigInt              = to - from + 1

  object Range:
    given parser: Parser[Range] = ((Parser.parseBigInt(10) <* "-") <*> Parser.parseBigInt(10)).map(Range.apply)

  case class Input(ranges: List[Range], ids: List[Id])

  object Input:
    given Parser[Input] = for
      ranges <- Parser.separatedBy("\n")(Range.parser)
      _      <- Parser.eatNewLine *> Parser.eatNewLine
      ids    <- Parser.separatedBy("\n")(Parser.parseBigInt(10))
    yield Input(ranges, ids)

  def part1(in: String): Task[P1] = ZIO.attempt {
    val input = ImplicitParser[Input].unsafeParse(in)

    input.ids.count(id => input.ranges.exists(_.contains(id)))
  }

  def part2(in: String): Task[P2] = ZIO.attempt {
    val input = ImplicitParser[Input].unsafeParse(in)

    val ranges: List[Range]  = input.ranges
    val sorted: List[Range]  = ranges.sortBy(r => r.from -> r.to)
    val nonOverlappingRanges = sorted.tail.foldLeft(List(sorted.head))((prevRanges, range) =>
      val last = prevRanges.last
      if range.from > last.to then prevRanges.appended(range)
      else prevRanges.dropRight(1).appended(Range(last.from, last.to max range.to))
    )
    nonOverlappingRanges.map(_.size).sum
  }

  val cases: List[DayCase[P1, P2]] = List(
    Test(
      "example",
      InputString("""3-5
                    |10-14
                    |16-20
                    |12-18
                    |
                    |1
                    |5
                    |8
                    |11
                    |17
                    |32""".stripMargin),
      3,
      14
    ),
    Puzzle(ResourceInput("day5puzzle.txt"), 885, BigInt("348115621205535"))
  )
end Day5
