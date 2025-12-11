package aoc.day

import zio.*

import aoc.common.ImplicitParser.{*, given}
import aoc.common.Input.{InputString, ResourceInput}
import aoc.common.Parser
import aoc.day.DayCase.{Puzzle, Test}
import neotype.Newtype

object Day11 extends Day:
  type P1 = Int
  type P2 = Int

  type Vertex = Vertex.Type
  object Vertex extends Newtype[String]:
    given parser: Parser[Vertex] = Parser.parseStringOfLen(3).map(Vertex.apply)

  case class Graph(adj: Map[Vertex, List[Vertex]]):
    def numberOfPaths(from: Vertex, to: Vertex): Long =
      val revAdj                                                                           = adj.toList.flatMap((v, ns) => ns.map(_ -> v)).groupMapReduce(_._1)(x => List(x._2))(_ ++ _)
      def bfs(frontier: Set[Vertex], waysToGetToEnd: Map[Vertex, Long]): Map[Vertex, Long] =
        if frontier.isEmpty then waysToGetToEnd
        else
          val newFrontier = for
            v <- frontier
            n <- revAdj(v)
          yield v

  object Graph:
    given parser: Parser[Graph] = Parser
      .separatedBy("\n")(
        Vertex.parser <* Parser.eatString(": ") <*> Parser.separatedBy(" ")(Vertex.parser)
      )
      .map(_.toMap)
      .map(Graph.apply)

  def part1(in: String): Task[P1] = ZIO.attempt {
    val graph = Graph.parser.unsafeParse(in)

    ???
  }

  def part2(in: String): Task[P2] = ZIO.attempt {
    ???
  }

  val cases: List[DayCase[P1, P2]] = List(
    Test(
      "example",
      InputString("""aaa: you hhh
                    |you: bbb ccc
                    |bbb: ddd eee
                    |ccc: ddd eee fff
                    |ddd: ggg
                    |eee: out
                    |fff: out
                    |ggg: out
                    |hhh: ccc fff iii
                    |iii: out""".stripMargin),
      5
    ),
    Puzzle(ResourceInput("day11puzzle.txt"))
  )
end Day11
