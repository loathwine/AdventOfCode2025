package aoc.day

import zio.*

import aoc.common.{Parser, Util}
import aoc.common.ImplicitParser.given
import aoc.common.Input.{InputString, ResourceInput}
import aoc.common.Vector.{BoundingBox, Dir, Pos}
import aoc.day.DayCase.{Puzzle, Test}
import scala.annotation.tailrec

object Day7 extends Day:
  type P1 = Int
  type P2 = BigInt

  case class Problem(start: Pos, splits: Set[Pos]):
    private val bb: BoundingBox = aoc.common.Vector.findBoundingBoxIncl(splits + start)

    private var memo: Map[Pos, BigInt] = Map.empty

    private def maybeSplit(p: Pos): Set[Pos] =
      if splits.contains(p) then Set(p + Dir.West, p + Dir.East)
      else Set(p)

    @tailrec
    final def fill(frontier: Set[Pos], acc: Set[Pos]): Set[Pos] =
      if frontier.isEmpty then acc
      else
        val nextFrontier = for
          p      <- frontier
          nextPos = p + Dir.South
          if bb.containsIncl(nextPos)
          next   <- maybeSplit(nextPos)
        yield next
        fill(nextFrontier, acc ++ nextFrontier)

    // Swinging dynamic programming hammer
    final def timelines(tacheon: Pos): BigInt =
      if memo.contains(tacheon) then memo(tacheon)
      else
        val tl =
          if !bb.containsIncl(tacheon) then BigInt(1)
          else
            val nextPos = tacheon + Dir.South
            val next    = maybeSplit(nextPos)
            next.toList.map(timelines).sum

        memo = memo.updated(tacheon, tl)
        tl
  end Problem

  object Problem:
    given parser: Parser[Problem] = Util
      .mapParser(Parser.parseChar('S') or Parser.parseChar('^'), Some(Parser.parseChar('.')))
      .map(m => Problem(m.find(_._2 == 'S').get._1, m.filter(_._2 == '^').keySet))

  def part1(in: String): Task[P1] = ZIO.attempt {
    val problem      = Problem.parser.unsafeParse(in)
    val firstBeamPos = Set(problem.start + Dir.South)
    val beams        = problem.fill(firstBeamPos, firstBeamPos)
    val splits       = problem.splits.filter(p => beams.contains(p + Dir.North))
    splits.size
  }

  def part2(in: String): Task[P2] = ZIO.attempt {
    val problem      = Problem.parser.unsafeParse(in)
    val firstBeamPos = problem.start + Dir.South
    problem.timelines(firstBeamPos)
  }

  val cases: List[DayCase[P1, P2]] = List(
    Test(
      "example",
      InputString(""".......S.......
                    |...............
                    |.......^.......
                    |...............
                    |......^.^......
                    |...............
                    |.....^.^.^.....
                    |...............
                    |....^.^...^....
                    |...............
                    |...^.^...^.^...
                    |...............
                    |..^...^.....^..
                    |...............
                    |.^.^.^.^.^...^.
                    |...............""".stripMargin),
      21,
      40
    ),
    Puzzle(ResourceInput("day7puzzle.txt"), 1518, BigInt("25489586715621"))
  )
end Day7
