package aoc.day

import zio.*

import aoc.common.ImplicitParser.{*, given}
import aoc.common.Input.{InputString, ResourceInput}
import aoc.common.Parser
import aoc.day.DayCase.{Puzzle, Test}
import neotype.Newtype

object Day11 extends Day:
  type P1 = Int
  type P2 = Long

  type Vertex = Vertex.Type
  object Vertex extends Newtype[String]:
    given parser: Parser[Vertex] = Parser.parseStringOfLen(3).map(Vertex.apply)

  case class Graph(adj: Map[Vertex, List[Vertex]]):
    def numberOfPaths(from: Vertex, to: Vertex): Int =
      var memo: Map[Vertex, Int]        = Map.empty
      def pathsToEnd(from: Vertex): Int =
        if memo.contains(from) then memo(from)
        else
          val res =
            if from == to then 1
            else if !adj.contains(from) then 0
            else adj(from).map(pathsToEnd).sum
          memo = memo.updated(from, res)
          res
      pathsToEnd(from)

  object Graph:
    given parser: Parser[Graph] = Parser
      .separatedBy("\n")(
        Vertex.parser <* Parser.eatString(": ") <*> Parser.separatedBy(" ")(Vertex.parser)
      )
      .map(_.toMap)
      .map(Graph.apply)

  def part1(in: String): Task[P1] = ZIO.attempt {
    val graph = Graph.parser.unsafeParse(in)
    graph.numberOfPaths(Vertex("you"), Vertex("out"))
  }

  def part2(in: String): Task[P2] = ZIO.attempt {
    val graph       = Graph.parser.unsafeParse(in)
    val fft         = Vertex("fft")
    val dac         = Vertex("dac")
    val fftToDac    = graph.numberOfPaths(fft, dac)
    val dacToFft    = graph.numberOfPaths(dac, fft)
    val middlePaths = fftToDac.max(dacToFft)
    val (m1, m2)    = if fftToDac > 0 then (fft, dac) else (dac, fft)
    val svrToM1     = graph.numberOfPaths(Vertex("svr"), m1)
    val m2ToOut     = graph.numberOfPaths(m2, Vertex("out"))
    svrToM1.toLong * middlePaths.toLong * m2ToOut.toLong
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
    Test.applyP2(
      "example2",
      InputString("""svr: aaa bbb
                    |aaa: fft
                    |fft: ccc
                    |bbb: tty
                    |tty: ccc
                    |ccc: ddd eee
                    |ddd: hub
                    |hub: fff
                    |eee: dac
                    |dac: fff
                    |fff: ggg hhh
                    |ggg: out
                    |hhh: out""".stripMargin),
      2
    ),
    Puzzle(ResourceInput("day11puzzle.txt"), 466)
  )
end Day11
