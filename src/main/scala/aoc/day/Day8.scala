package aoc.day

import zio.*

import aoc.common.ImplicitParser.{*, given}
import aoc.common.Input.{InputString, ResourceInput}
import aoc.common.Parser
import aoc.day
import aoc.day.DayCase.{Puzzle, Test}
import neotype.Subtype
import neotype.common.NonEmptyString
import scala.annotation.tailrec

object Day8 extends Day:
  type P1 = Int
  type P2 = Int

  case class Pos3d(x: Int, y: Int, z: Int):
    def -(p: Pos3d): Pos3d = Pos3d(x - p.x, y - p.y, z - p.z)
    def length: Double     = math.sqrt(x * x + y * y + z * z)

  object Pos3d:
    given parser: Parser[Pos3d] = for
      x <- Parser.parseInt(10) <* ","
      y <- Parser.parseInt(10) <* ","
      z <- Parser.parseInt(10)
    yield Pos3d(x, y, z)

  type Junction = Junction.Type
  object Junction extends Subtype[Pos3d]

  case class Problem(take: Int, junctions: List[Junction]):
    // This is union-find ish
    @tailrec
    private def findCircuits(
      combine: List[(Junction, Junction)],
      circuitMap: Map[Junction, Junction]
    ): Set[Set[Junction]] =
      combine match
        case Nil              =>
          circuitMap.groupBy(_._2).map(_._2.keySet).toSet
        case (j1, j2) :: tail =>
          val j1to          = circuitMap(j1)
          val j2to          = circuitMap(j2)
          val newCircuitMap =
            if j1to == j2to then circuitMap
            else
              // Merge j1to and j2to
              val s1 = circuitMap.filter(_._2 == j1to).keySet
              val s2 = circuitMap.filter(_._2 == j2to).keySet
              if s1.size > s2.size then circuitMap ++ s2.map(_ -> j1to).toMap
              else circuitMap ++ s1.map(_ -> j2to).toMap
          findCircuits(tail, newCircuitMap)

    def findAllCircuits(combine: List[(Junction, Junction)]): Set[Set[Junction]] =
      findCircuits(combine, junctions.map(x => x -> x).toMap)

  end Problem

  object Problem:
    given parser: Parser[Problem] =
      (Parser.parseInt(10) <* Parser.eatNewLine <*> Parser.separatedBy("\n")(Pos3d.parser.map(Junction.apply)))
        .map(Problem.apply)

  def part1(in: String): Task[P1] = ZIO.attempt {
    val problem  = Problem.parser.unsafeParse(in)
    val circuits = problem.findAllCircuits(
      problem.junctions
        .combinations(2)
        .map(l => l.head -> l(1))
        .toList
        .sortBy((j1, j2) => (j1 - j2).length)
        .take(problem.take)
    )
    circuits.toList.map(_.size).sorted.reverse.take(3).product
  }

  def part2(in: String): Task[P2] = ZIO.attempt {
    ???
  }

  val cases: List[DayCase[P1, P2]] = List(
    Test(
      "example",
      InputString("""10
                    |162,817,812
                    |57,618,57
                    |906,360,560
                    |592,479,940
                    |352,342,300
                    |466,668,158
                    |542,29,236
                    |431,825,988
                    |739,650,466
                    |52,470,668
                    |216,146,977
                    |819,987,18
                    |117,168,530
                    |805,96,715
                    |346,949,466
                    |970,615,88
                    |941,993,340
                    |862,61,35
                    |984,92,344
                    |425,690,689""".stripMargin),
      40
    ),
    Puzzle(ResourceInput("day8puzzle.txt"))
  )
end Day8
