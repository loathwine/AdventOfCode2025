package aoc.common

import doodle.java2d.Picture

object Vector:

  case class BigPos(x: BigInt, y: BigInt):
    def +(direction: Dir): BigPos = BigPos(x + direction.x, y + direction.y)

    def -(p: BigPos): BigPos = BigPos(x - p.x, y - p.y)

  case class Pos(x: Int, y: Int):
    def +(direction: Dir): Pos = Pos(x + direction.x, y + direction.y)
    def -(p: Pos): Pos         = Pos(x - p.x, y - p.y)
    def length: Double         = math.sqrt(x * x + y * y)
    def toDir: Dir             = Dir(x, y)

  case class Dir(x: Int, y: Int):
    def +(direction: Dir): Dir = Dir(x + direction.x, y + direction.y)
    def unary_-                = Dir(-x, -y)
    def *(i: Int)              = Dir(x * i, y * i)
    def cross(other: Dir): Int = x * other.y - y * other.x

    def rotated90CCW: Dir =
      val pc    = PolarCoordinate.make(Pos(x, y))
      val newPc = pc.rotated(math.Pi / 2)
      newPc.toCartesianRounded.toDir

    def rotated90CW: Dir =
      val pc    = PolarCoordinate.make(Pos(x, y))
      val newPc = pc.rotated(-math.Pi / 2)
      newPc.toCartesianRounded.toDir

    def sign: Dir = Dir(x.sign, y.sign)
  end Dir

  case class PolarCoordinate(r: Double, theta: Double):

    def toCartesianRounded: Pos =
      val (x, y) = toCartesian
      Pos(x.round.toInt, y.round.toInt)

    def toCartesian: (Double, Double) = (r * math.cos(theta), r * math.sin(theta))

    def rotated(radians: Double): PolarCoordinate = this.copy(theta = theta + radians)

  object PolarCoordinate:

    def make(p: Pos): PolarCoordinate =
      PolarCoordinate(p.length, math.atan2(p.y, p.x))

  def dist(p1: Pos, p2: Pos): Double = (p1 - p2).length

  object Dir:
    val North: Dir     = Dir(0, 1)
    val NorthEast: Dir = Dir(1, 1)
    val East: Dir      = Dir(1, 0)
    val SouthEast: Dir = Dir(1, -1)
    val South: Dir     = Dir(0, -1)
    val SouthWest: Dir = Dir(-1, -1)
    val West: Dir      = Dir(-1, 0)
    val NorthWest: Dir = Dir(-1, 1)

    val all4Dirs: List[Dir] = List(
      Dir.North,
      Dir.East,
      Dir.South,
      Dir.West
    )

    val all8Dirs: List[Dir] = List(
      Dir.North,
      Dir.NorthEast,
      Dir.East,
      Dir.SouthEast,
      Dir.South,
      Dir.SouthWest,
      Dir.West,
      Dir.NorthWest
    )
  end Dir

  case class BoundingBox(min: Pos, max: Pos):
    def containsIncl(p: Pos): Boolean = !(p.x < min.x || p.y < min.y || p.x > max.x || p.y > max.y)

    def topLeft: Pos = Pos(min.x, max.y)

    def bottomRight: Pos = Pos(max.x, min.y)

  // Returns (min, max) where min = Pos(minX, minY) and max = Pos(maxX, maxY)
  def findBoundingBoxIncl(positions: Set[Pos]): BoundingBox =
    val minX = positions.map(_.x).min
    val maxX = positions.map(_.x).max
    val minY = positions.map(_.y).min
    val maxY = positions.map(_.y).max
    BoundingBox(Pos(minX, minY), Pos(maxX, maxY))

  def findBoundingBoxExcl(positions: Set[Pos]): BoundingBox =
    val minX = positions.map(_.x).min - 1
    val maxX = positions.map(_.x).max + 1
    val minY = positions.map(_.y).min - 1
    val maxY = positions.map(_.y).max + 1
    BoundingBox(Pos(minX, minY), Pos(maxX, maxY))

  def isClockwise(simpleUpDownLeftRightCycle: List[Pos]): Boolean =
    val cycle = simpleUpDownLeftRightCycle
    if cycle.size < 4 then throw new IllegalArgumentException(s"Cycle must have at least 4 elements")
    else
      val cycleWithoutDuplicateBounds =
        if cycle.head == cycle.last then cycle.dropRight(1)
        else cycle

      // Angle of 3 consecutive points in the cycle.
      def angle(p1: Pos, p2: Pos, p3: Pos): Int =
        val d1 = (p2 - p1).toDir
        val d2 = (p3 - p2).toDir

        if d1 == d2 then 0
        else if d2 == d1.rotated90CW then 90
        else if d2 == d1.rotated90CCW then -90
        else throw new IllegalArgumentException(s"Unexpected direction change in path $d1 $d2")

      val angleSum = cycleWithoutDuplicateBounds
        .appendedAll(cycle.take(2))
        .sliding(3)
        .map {
          case List(p1, p2, p3) =>
            angle(p1, p2, p3)
          case _                => ???
        }
        .sum

      if angleSum == 360 then true
      else if angleSum == -360 then false
      else throw new Throwable(s"Unexpected angle sum in loop, $angleSum")
    end if
  end isClockwise

  def clockwiseVersion(simpleUpDownLeftRightCycle: List[Pos]): List[Pos] =
    if isClockwise(simpleUpDownLeftRightCycle) then simpleUpDownLeftRightCycle else simpleUpDownLeftRightCycle.reverse

  def areaOfPolygon(uniqueVertices: List[BigPos]): BigDecimal =
    val vectors    = uniqueVertices
    val sum        = vectors
      .appended(vectors.head)
      .sliding(2)
      .map {
        case List(a, b) =>
          a.x * b.y - a.y * b.x
        case _          => ???
      }
      .sum
    val signedArea = BigDecimal(sum) / 2
    signedArea.abs
end Vector
