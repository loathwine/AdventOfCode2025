package aoc.day

import zio.*

import DayCase.{Puzzle, Test}
import aoc.common.ImplicitParser.{*, given}
import aoc.common.Input.{InputString, ResourceInput}
import aoc.common.Parser

object DayX extends Day:
  type P1 = Int
  type P2 = Int

  def part1(in: String): Task[P1] = ZIO.attempt {
    ???
  }

  def part2(in: String): Task[P2] = ZIO.attempt {
    ???
  }

  val cases: List[DayCase[P1, P2]] = List(
    Test("example", InputString("""""")),
    Puzzle(ResourceInput("dayXpuzzle.txt"))
  )
