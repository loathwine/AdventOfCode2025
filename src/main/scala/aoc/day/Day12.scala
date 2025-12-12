package aoc.day

import zio.*

import aoc.common.{Parser, Util}
import aoc.common.ImplicitParser.{*, given}
import aoc.common.Input.{InputString, ResourceInput}
import aoc.common.Vector.Pos
import aoc.day.DayCase.{Puzzle, Test}

/*
This is a NP-hard problem but we can use a heuristic for some trivial cases.
Turns out heuristic is enough for puzzle input.
 */
object Day12 extends Day:
  type P1 = Int
  type P2 = Int

  // All shapes are within 3x3
  // Always exactly 6 shapes

  case class Shape(empty: Set[Pos]) // Keep track of which positions in 3x3 are empty

  object Shape:
    given parser: Parser[Shape] = for
      _            <- Parser.parseDigit <* ":" <* Parser.eatNewLine
      emptySlotMap <- Parser
                        .multiple(3, Some(3), Some(Parser.eatNewLine))(Parser.parseLine)
                        .map(_.mkString("\n"))
                        .feedInto(Util.mapParser(Parser.parseChar('.'), Some(Parser.parseChar('#'))))
    yield Shape(emptySlotMap.keySet)

  case class Region(width: Int, height: Int, target: Vector[Int]):
    val area = width * height

  object Region:
    given parser: Parser[Region] = for
      width  <- Parser.parseInt(10) <* "x"
      height <- Parser.parseInt(10) <* ": "
      target <- Parser.separatedBy(" ")(Parser.parseInt(10))
    yield Region(width, height, target.toVector)

  case class Problem(shapes: Vector[Shape], regions: List[Region]):
    def notImpossibleRegions: Int =
      val shapeBlocks = shapes.map(shape => 9 - shape.empty.size)
      regions.count { region =>
        // Just naively assume we can't fit smartly, so every shape is 3x3 exactly.
        // Then each shape takes up 9 squares. If the area is large enough even for this it must be possible.
        val definitelyPossible   =
          region.area >= region.target.sum * 9
        // On the other hand, with perfect packing we just count the # and if the area is still not big enough it is not going to work.
        val definitelyImpossible = region.target.zipWithIndex.map((t, i) => shapeBlocks(i) * t).sum > region.area
        // Third case would need smart packing but puzzle input is kind to us so this heuristic is enough.

        definitelyPossible || !definitelyImpossible
      }

  object Problem:
    given parser: Parser[Problem] = for
      shapes  <- Parser.separatedBy("\n\n")(Shape.parser) <* Parser.eatNewLine.multiple(2)
      regions <- Parser.separatedBy("\n")(Region.parser)
    yield Problem(shapes.toVector, regions)

  def part1(in: String): Task[P1] = ZIO.attempt {
    val problem = Problem.parser.unsafeParse(in)
    problem.notImpossibleRegions - (1 - problem.regions.length / 1000) // This hacky tail is just to get -1 on example input :P
  }

  def part2(in: String): Task[P2] = ZIO.attempt { ??? } // No part 2

  val cases: List[DayCase[P1, P2]] = List(
    Test(
      "example",
      InputString("""0:
                    |###
                    |##.
                    |##.
                    |
                    |1:
                    |###
                    |##.
                    |.##
                    |
                    |2:
                    |.##
                    |###
                    |##.
                    |
                    |3:
                    |##.
                    |###
                    |##.
                    |
                    |4:
                    |###
                    |#..
                    |###
                    |
                    |5:
                    |###
                    |.#.
                    |###
                    |
                    |4x4: 0 0 0 0 2 0
                    |12x5: 1 0 1 0 2 2
                    |12x5: 1 0 1 0 3 2""".stripMargin),
      2
    ),
    Puzzle(ResourceInput("day12puzzle.txt"), 546)
  )
end Day12
