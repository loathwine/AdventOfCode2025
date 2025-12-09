package aoc.day

import zio.*

import aoc.common.ImplicitParser.{*, given}
import aoc.common.Input.{InputString, ResourceInput}
import aoc.common.Parser
import aoc.common.Vector.Pos
import aoc.day.DayCase.{Puzzle, Test}

object Day9 extends Day:
  type P1 = Long
  type P2 = Int

  case class Problem(points: List[Pos])

  object Problem:
    given parser: Parser[Problem] =
      Parser.separatedBy("\n")((Parser.parseInt(10) <* "," <*> Parser.parseInt(10)).map(Pos.apply)).map(Problem.apply)

  def rectangleArea(corners: (Pos, Pos)): Long =
    val (left, right) = if corners._1.x <= corners._2.x then corners else corners.swap
    val width         = (right.x.toLong - left.x.toLong).abs + 1
    val height        = (right.y.toLong - left.y.toLong).abs + 1
    width * height

  def part1(in: String): Task[P1] = ZIO.attempt {
    val problem = Problem.parser.unsafeParse(in)
    problem.points
      .combinations(2)
      .map { case List(p1, p2) =>
        rectangleArea(p1 -> p2)
      }
      .max
  }

  def part2(in: String): Task[P2] = ZIO.attempt {
    ???
  }

  val cases: List[DayCase[P1, P2]] = List(
    Test(
      "example",
      InputString("""7,1
                    |11,1
                    |11,7
                    |9,7
                    |9,5
                    |2,5
                    |2,3
                    |7,3""".stripMargin),
      50
    ),
    Puzzle(ResourceInput("day9puzzle.txt"), 4746238001L)
  )
end Day9
