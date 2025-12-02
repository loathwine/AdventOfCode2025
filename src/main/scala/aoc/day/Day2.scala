package aoc.day

import zio.*

import aoc.common.ImplicitParser.{*, given}
import aoc.common.Input.{InputString, ResourceInput}
import aoc.common.Parser
import aoc.day.DayCase.{Puzzle, Test}
import scala.annotation.tailrec

// Part 1: Tried to do something smart
// Part 2: Basically brute force
object Day2 extends Day:
  type P1 = BigInt
  type P2 = BigInt
  case class Range(from: Long, to: Long)

  object Range:
    given parser: Parser[Range] =
      for
        from <- Parser.parseLong(10) <* "-"
        to   <- Parser.parseLong(10)
      yield Range(from, to)

  val parseRanges = Parser.separatedBy(",")(Range.parser)

  @tailrec
  def invalidIds(range: Range, acc: BigInt): BigInt =
    if range.from > range.to then acc
    else
      val digits    = range.from.toString
      val dLen      = digits.length
      val nextDigit = ("1" + "0" * dLen).toLong
      if dLen % 2 != 0 then invalidIds(Range(nextDigit, range.to), acc)
      else
        // 1234 5678
        // 1234 7965
        val top      = (nextDigit - 1).min(range.to)
        val bot      = range.from
        val halfBot  = digits.take(dLen / 2).toLong
        val invalids = LazyList
          .iterate(halfBot)(_ + 1)
          .map(l => (l.toString + l.toString).toLong)
          .dropWhile(_ < bot)
          .takeWhile(_ <= top)
          .toList

        val sumInvalid = invalids.map(BigInt(_)).sum
        invalidIds(Range(nextDigit, range.to), acc + sumInvalid)

  def part1(in: String): Task[P1] = ZIO.attempt {
    val ranges = parseRanges.unsafeParse(in)
    ranges.map(invalidIds(_, 0)).sum
  }

  private def isInvalid(id: Long): Boolean =
    val digits = id.toString
    val dLen   = digits.length

    def repeats(groupSize: Int): Boolean =
      dLen % groupSize == 0 && digits.grouped(groupSize).toSet.size == 1

    val half = dLen / 2
    (1 to half).exists(repeats)

  def part2(in: String): Task[P2] = ZIO.attempt {
    val ranges = parseRanges.unsafeParse(in)
    ranges.map(r => (r.from to r.to).filter(isInvalid).map(BigInt(_)).sum).sum
  }

  val cases: List[DayCase[P1, P2]] = List(
    Test(
      "example",
      InputString(
        "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
      ),
      BigInt(1227775554L),
      BigInt(4174379265L)
    ),
    Puzzle(ResourceInput("day2puzzle.txt"), BigInt(20223751480L), BigInt(30260171216L))
  )
end Day2
