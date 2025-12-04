package aoc.day

import zio.*

import aoc.common.{Parser, Util}
import aoc.common.ImplicitParser.{*, given}
import aoc.common.Input.{InputString, ResourceInput}
import aoc.common.Vector.{Dir, Pos}
import aoc.day.DayCase.{Puzzle, Test}

object Day4 extends Day:
  type P1 = Int
  type P2 = Int

  enum Tile:
    case Empty
    case PaperRoll

  object Tile:
    given parser: Parser[Tile.PaperRoll.type] = Parser.parseChar('@').as(Tile.PaperRoll)

  private def rollsThatCanBeRemoved(rolls: Set[Pos]): Set[Pos] =
    rolls.filter(p => Dir.all8Dirs.count(d => rolls.contains(p + d)) < 4)

  def part1(in: String): Task[P1] = ZIO.attempt {
    val map        = Util.mapParser(Tile.parser, ignore = Some(Parser.parseChar('.'))).unsafeParse(in)
    val paperRolls = map.keySet
    rollsThatCanBeRemoved(paperRolls).size
  }

  def part2(in: String): Task[P2] = ZIO.attempt {
    val map        = Util.mapParser(Tile.parser, ignore = Some(Parser.parseChar('.'))).unsafeParse(in)
    val paperRolls = map.keySet
    LazyList
      .iterate(paperRolls -> 0) { (rolls, _) =>
        val removed = rollsThatCanBeRemoved(rolls)
        (rolls -- removed, removed.size)
      }
      .tail
      .takeWhile(_._2 > 0)
      .map(_._2)
      .sum
  }

  val cases: List[DayCase[P1, P2]] = List(
    Test(
      "example",
      InputString("""..@@.@@@@.
                    |@@@.@.@.@@
                    |@@@@@.@.@@
                    |@.@@@@..@.
                    |@@.@@@@.@@
                    |.@@@@@@@.@
                    |.@.@.@.@@@
                    |@.@@@.@@@@
                    |.@@@@@@@@.
                    |@.@.@@@.@.""".stripMargin),
      13,
      43
    ),
    Puzzle(ResourceInput("day4puzzle.txt"), 1578)
  )
end Day4
