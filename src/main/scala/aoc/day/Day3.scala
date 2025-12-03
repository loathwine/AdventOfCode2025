package aoc.day

import zio.*

import aoc.common.ImplicitParser.{*, given}
import aoc.common.Input.{InputString, ResourceInput}
import aoc.common.Parser
import aoc.day.DayCase.{Puzzle, Test}
import scala.annotation.tailrec

object Day3 extends Day:
  type P1    = Int
  type P2    = Long
  type Digit = Int

  val inputParser: Parser[List[List[Digit]]] =
    Parser.separatedBy("\n")(Parser.multiple(1, None, None)(Parser.parseDigit))

  def bestCombo(digits: List[Digit]): List[(Digit, Digit)] =
    def helper(rem: List[Digit], maxPrev: Digit): List[(Digit, Digit)] = rem match
      case head :: tail =>
        (maxPrev, head) :: helper(tail, maxPrev max head)
      case Nil          => Nil
    helper(digits, 0)

  def part1(in: String): Task[P1] = ZIO.attempt {
    val digitRows = inputParser.unsafeParse(in)
    digitRows.map(bestCombo).map(_.map((d1, d2) => d1 * 10 + d2).max).sum
  }

  case class Solver(digits: Vector[Digit]):
    type Index = Int
    type Rem   = Int
    val dLen                              = digits.length
    var memo: Map[(Index, Rem), Long]     = Map.empty
    def solve(idx: Index, rem: Rem): Long =
      memo.get(idx -> rem) match
        case Some(best) => best
        case None       =>
          val best: Long =
            val remDigits = dLen - idx
            if remDigits <= rem then digits.takeRight(remDigits).mkString.toLong // Luckily data doesn't contain 0
            else
              val curr           = digits(idx)
              // Either we include current or not
              val included: Long =
                if rem == 1 then curr else (curr.toString + solve(idx + 1, rem - 1).toString).toLong
              val excluded       = solve(idx + 1, rem)
              included max excluded
          memo = memo.updated((idx, rem), best)
          best
    def solve(combined: Int): Long        = solve(0, combined)
  end Solver

  def part2(in: String): Task[P2] = ZIO.attempt {
    val digits  = inputParser.unsafeParse(in)
    val solvers = digits.map(d => Solver(d.toVector))
    val best    = solvers.map(_.solve(12))
    best.sum
  }

  val cases: List[DayCase[P1, P2]] = List(
    Test(
      "example",
      InputString("""987654321111111
                    |811111111111119
                    |234234234234278
                    |818181911112111""".stripMargin),
      357,
      3121910778619L
    ),
    Puzzle(ResourceInput("day3puzzle.txt"), 16812, 166345822896410L)
  )
end Day3
