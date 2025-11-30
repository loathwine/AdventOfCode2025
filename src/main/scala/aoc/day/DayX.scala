package aoc.day

import zio.*

import DayCase.{Puzzle, Test}
import aoc.common.ImplicitParser.{*, given}
import aoc.common.Input.{InputString, ResourceInput}
import aoc.common.Parser

object DayX extends Day[Int, Int]:

  def part1(in: String): Task[Int] = ZIO.attempt {
    ???
  }

  def part2(in: String): Task[Int] = ZIO.attempt {
    ???
  }

  val cases: List[DayCase[Int, Int]] = List(
    Test("example", InputString("""""")),
    Puzzle(ResourceInput("dayXpuzzle.txt"))
  )
