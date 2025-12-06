package aoc.day

import zio.*

import aoc.common.ImplicitParser.{*, given}
import aoc.common.Input.{InputString, ResourceInput}
import aoc.common.Parser
import aoc.day.DayCase.{Puzzle, Test}

object Day6 extends Day:
  type P1 = BigInt
  type P2 = BigInt

  enum Op:
    case Add
    case Mul

  object Op:
    given parser: Parser[Op] = Parser.charMap('+' -> Op.Add, '*' -> Op.Mul)
  end Op

  case class Problem(rows: List[List[BigInt]], ops: List[Op])

  object Problem:
    private def parseLine[A](p: Parser[A]): Parser[List[A]] =
      Parser.eatSpaces.option *> Parser.multiple(1, None, Some(Parser.eatSpaces))(p) <* Parser.eatSpaces.option
    given parser: Parser[Problem]                           =
      for
        rows <- Parser.separatedBy("\n")(parseLine(Parser.parseBigInt(10)))
        _    <- Parser.eatNewLine
        ops  <- parseLine(Op.parser)
      yield Problem(rows, ops)
  end Problem

  def part1(in: String): Task[P1] = ZIO.attempt {
    val problem = Problem.parser.unsafeParse(in)
    problem.rows.transpose
      .zip(problem.ops)
      .map((nums, op) =>
        op match
          case Op.Add => nums.sum
          case Op.Mul => nums.product
      )
      .sum
  }

  case class P2Comb(nums: List[BigInt], op: Op)

  object P2Comb:
    given parser: Parser[P2Comb] = for
      n1   <- Parser.eatSpaces.option *> Parser.parseBigInt(10) <* Parser.eatSpaces.option
      op   <- Op.parser <* Parser.eatNewLine
      rest <- Parser.separatedBy("\n")(Parser.eatSpaces.option *> Parser.parseBigInt(10) <* Parser.eatSpaces.option)
    yield P2Comb(n1 :: rest, op)
  end P2Comb

  def part2(in: String): Task[P2] = ZIO.attempt {
    val transposed      = in.split("\n").map(_.toCharArray).transpose
    val transposedInput = transposed.map(_.mkString).mkString("\n")
//     println(transposedInput)
    // When we transpose we actually transform into a different input
    /* Example
        1  *
        24
        356

        369+
        248
        8

         32*
        581
        175

        623+
        431
          4
     */
    val combs           = Parser
      .multiple(1, None, Some(Parser.eatNewLine *> Parser.eatSpaces *> Parser.eatNewLine))(P2Comb.parser)
      .unsafeParse(transposedInput)
    val results         = combs.map(c =>
      c.op match
        case Op.Add => c.nums.sum
        case Op.Mul => c.nums.product
    )
    results.sum
  }

  val cases: List[DayCase[P1, P2]] = List(
    Test("example", ResourceInput("day6example.txt"), BigInt("4277556"), BigInt("3263827")), // Put in file because we need trailing spaces. Formatting removes it.
    Puzzle(ResourceInput("day6puzzle.txt"), BigInt("5667835681547"), BigInt("9434900032651"))
  )
end Day6
