package aoc.day

import zio.*
import zio.prelude.NonEmptyList

import aoc.common.ImplicitParser.{*, given}
import aoc.common.Input.{InputString, ResourceInput}
import aoc.common.Parser
import aoc.common.Util.posMod
import aoc.day.DayCase.{Puzzle, Test}

object Day1 extends Day:
  type P1 = Int
  type P2 = Int

  enum RotateDir:
    case Left
    case Right

  case class Rotation(dir: RotateDir, num: Int)
  object Rotation:
    given parser: Parser[Rotation] = for
      dir <- Parser.charMap('L' -> RotateDir.Left, 'R' -> RotateDir.Right)
      num <- Parser.parseInt(10)
    yield Rotation(dir, num)

  val parseInput: Parser[List[Rotation]]                      = Parser.separatedBy("\n")(Rotation.parser)
  def rotateM(limit: Int)(curr: Int, rotation: Rotation): Int =
    val change = rotation.dir match
      case RotateDir.Left  => -rotation.num
      case RotateDir.Right => rotation.num
    val next   = posMod(curr + change, limit)
    next

  def part1(in: String): Task[P1] = ZIO.attempt {
    val rotations = parseInput.unsafeParse(in)
    val rotate    = rotateM(100)
    val results   = rotations.scanLeft(50)(rotate)
    results.count(_ == 0)
  }

  def clicks(curr: Int, rotation: Rotation): (pos: Int, clicked: Boolean) =
    if rotation.num == 100 then (curr, true)
    else
      rotation.dir match
        case RotateDir.Left  =>
          val next = curr - rotation.num
          (posMod(next, 100), next <= 0 && curr != 0)
        case RotateDir.Right =>
          val next = curr + rotation.num
          (posMod(next, 100), next >= 100 && curr != 0)

  def part2(in: String): Task[P2] = ZIO.attempt {
    val rotations       = parseInput.unsafeParse(in)
    val simpleRotations = rotations.toList
      .flatMap(rotation =>
        (if rotation.num > 99 then List.fill(rotation.num / 100)(rotation.copy(num = 100)) else Nil)
          .appended(rotation.copy(num = rotation.num % 100))
      )
      .filterNot(_.num == 0)

    val results = simpleRotations.scanLeft((pos = 50, clicked = false))((x, rotation) => clicks(x.pos, rotation))
    results.count(_.clicked)
  }

  val cases: List[DayCase[P1, P2]] = List(
    Test(
      "example",
      InputString("""L68
                    |L30
                    |R48
                    |L5
                    |R60
                    |L55
                    |L1
                    |L99
                    |R14
                    |L82""".stripMargin),
      3,
      6
    ),
    Puzzle(ResourceInput("day1puzzle.txt"), 1074, 6254)
  )
end Day1
