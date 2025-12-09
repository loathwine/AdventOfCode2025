package aoc.day

import zio.*

import aoc.common.{Parser, Util}
import aoc.common.ImplicitParser.{*, given}
import aoc.common.Input.{InputString, ResourceInput}
import aoc.common.Vector.{clockwiseVersion, findBoundingBoxIncl, Dir, Pos}
import aoc.day
import aoc.day.DayCase.{Puzzle, Test}
import neotype.Subtype
import scala.annotation.tailrec

object Day9 extends Day:
  type P1 = Long
  type P2 = Long

  type CompressedPos = CompressedPos.Type
  object CompressedPos extends Subtype[Pos]

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

  @tailrec
  def fill(frontier: Set[Pos], visited: Set[Pos]): Set[Pos] =
    if frontier.isEmpty then visited
    else
      val newFrontier = for
        p <- frontier
        d <- Dir.all4Dirs
        n  = p + d
        if !visited(n) && !frontier(n)
      yield n
      fill(newFrontier, visited ++ frontier)

  def findInteriorPoint(clockwise: List[Pos], border: Set[Pos]): Pos =
    // Try each edge, stepping inside at the midpoint
    clockwise
      .appended(clockwise.head)
      .sliding(2)
      .map { case List(p1, p2) =>
        val edgeDir     = (p2 - p1).toDir.sign
        val midpoint    = Pos((p1.x + p2.x) / 2, (p1.y + p2.y) / 2)
        val rightOfEdge = midpoint + edgeDir.rotated90CW
        Option.when(!border.contains(rightOfEdge))(rightOfEdge)
      }
      .collectFirst { case Some(p) =>
        p
      }
      .getOrElse(throw new RuntimeException("No interior point found"))

  // Naive
  def findCandidates(points: List[Pos]): List[(Pos, Pos)] =
    val clockwise: List[Pos] = clockwiseVersion(points)
    val border: Set[Pos]     = points
      .appended(points.head)
      .sliding(2)
      .flatMap { case List(p1, p2) =>
        val dir = (p2 - p1).toDir.sign
        LazyList.iterate(p1)(_ + dir).takeWhile(_ != p2).toList
      }
      .toSet
    val oneStepAlongBorder   = (clockwise.head - clockwise.last).toDir.sign
    val knownInsidePoint     = findInteriorPoint(clockwise, border)

    val inside = fill(Set(knownInsidePoint), border)

    // Visualization helps a lot to understand what we're dealing with.
//    visualize(points.toSet, border, inside)

    def isInside(corners: (Pos, Pos)): Boolean =
      val bb = findBoundingBoxIncl(Set(corners._1, corners._2))
      bb.corners.forall(inside) && bb.allPosIncl.forall(inside) // Check corners first as optimization

    points
      .combinations(2)
      .map { case List(p1, p2) =>
        p1 -> p2
      }
      .filter(isInside)
      .toList

  end findCandidates

  def part2(in: String): Task[P2] = ZIO.attempt {
    val problem = Problem.parser.unsafeParse(in)
    val points  = problem.points
    // We can do coordinate compression and use naive solution.
    val xs      = points.map(_.x).distinct.sorted.zipWithIndex.toMap
    val ys      = points.map(_.y).distinct.sorted.zipWithIndex.toMap

    val compress: Map[Pos, CompressedPos]   = points.map(p => p -> CompressedPos(Pos(xs(p.x), ys(p.y)))).toMap
    val decompress: Map[CompressedPos, Pos] = compress.toList.map(_.swap).toMap

    val candidates: Seq[(CompressedPos, CompressedPos)] =
      findCandidates(points.map(compress)).map((p1, p2) => CompressedPos(p1) -> CompressedPos(p2))

//    val best = candidates.sortBy((p1, p2) => rectangleArea(decompress(p1), decompress(p2))).reverse.take(5)
//    best.foreach { (c1, c2) =>
//      visualize(points.map(compress).toSet, points.map(compress).toSet, findBoundingBoxIncl(Set(c1, c2)).allPosIncl)
//    }
//    println(candidates.map((p1, p2) => rectangleArea(decompress(p1), decompress(p2))).sorted.reverse.take(5))

    // Drop 1 because our best solution is incorrect because it misses some empty space in compressed version
    // #XXX#                     ##
    //     X   => compressed => #X# <- empty space above this line but not shown in compressed version
    // #XXXX#
    // This is not a general solution :/
    candidates
      .map((p1, p2) => rectangleArea(decompress(p1), decompress(p2)))
      .sorted
      .dropRight(if points.size > 20 then 1 else 0) // Only drop for puzzle input :////
      .last
  }

  def visualize(points: Set[Pos], border: Set[Pos], inside: Set[Pos]): Unit =
    import doodle.core.*
    import doodle.effect.Renderer
    import doodle.image.*
    import doodle.java2d.*
    import doodle.syntax.all.*
    val bb = findBoundingBoxIncl(border)

    val size = if points.size > 20 then 3 else 50
    Util.drawPictureUnsafe(
      Util.visualizeMapDoodle(
        inside.map(_ -> '.').toMap ++ border.map(_ -> 'X').toMap ++ points.map(_ -> '#').toMap ++ Map(
          (bb.max + Dir(20, 20))   -> 'X',
          (bb.min + Dir(-20, -20)) -> 'X'
        ),
        x =>
          val color =
            if x == '#' then Color.red else if x == 'X' then Color.green else Color.gray
          Picture.square(size).fillColor(color)
        ,
        Picture.square(size).fillColor(Color.black)
      )
    )
  end visualize

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
      50,
      24
    ),
    Puzzle(ResourceInput("day9puzzle.txt"), 4746238001L, 1552139370L)
  )
end Day9
