package aoc.common

import aoc.common.Vector.Pos
import doodle.java2d.*
import doodle.syntax.all.*
import scala.math.BigDecimal.RoundingMode
import scala.math.Ordering.Implicits.infixOrderingOps

object Util:

  extension (bd: BigDecimal)
    def roundedTo(fractionDigits: Int): BigDecimal = bd.setScale(fractionDigits, RoundingMode.HALF_UP)
    def roundedBigInt: BigInt                      = roundedTo(0).toBigInt

  extension [K, V](m: Map[K, V]) def mapValuesWith[A](f: V => A): Map[K, A] = m.view.mapValues(f).toMap

  extension [A](xs: List[A]) def frequency: Map[A, Int] = xs.groupBy(identity).mapValuesWith(_.size)

  case class CycleParams[A](startElement: A, startIndex: Int, cycleLength: Int):
    def cycleIdx(i: BigInt): Int = ((i - startIndex) % cycleLength).toInt
    def modIdx(i: BigInt): Int   = startIndex + cycleIdx(i)

  extension [A](lzy: LazyList[A])
    def getFirstRepeated: A = lzy.sliding(2).find(it => it.head == it.last).get.head

    def getFirstRepeatedWithIndex: (A, Int) = lzy.zipWithIndex.sliding(2).find(it => it.head._1 == it.last._1).get.head

    def getFirstCycle: CycleParams[A] = lzy.zipWithIndex
      .scanLeft(Map.empty[A, Int] -> Option.empty[CycleParams[A]]) { case ((seen, found), (a, idx)) =>
        if found.isDefined then seen -> found
        else if seen.contains(a) then seen -> Some(CycleParams(a, seen(a), idx - seen(a)))
        else seen.updated(a, idx)          -> None
      }
      .map(_._2)
      .collectFirst { case Some(cycleParams) =>
        cycleParams
      }
      .get

    def takeWhileNotRepeating: LazyList[A] = lzy.head #:: lzy
      .zip(lzy.drop(1))
      .takeWhile { case (a, b) =>
        a != b
      }
      .map(_._2)
  end extension

  // Assumes we're searching in a sorted list with value of type V.
  // Returns index of first element >= value.
  def lowerBound[V](firstIdxIncl: BigInt, lastIdxExcl: BigInt, value: V, valueAtIdx: BigInt => V)(using
    o: Ordering[V]
  ): BigInt =
    // Adjusted from https://en.cppreference.com/w/cpp/algorithm/lower_bound
    var f             = firstIdxIncl
    var count: BigInt = lastIdxExcl - firstIdxIncl

    while count > 0 do
      val step = count / 2
      val i    = f + step
      if valueAtIdx(i) < value then
        f = i + 1
        count -= step + 1
      else count = step

    f

  def time[R](block: => R): R =
    val t0     = System.currentTimeMillis()
    val result = block // call-by-name
    val t1     = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result

  def posMod(a: BigInt, m: BigInt): BigInt = (a % m + m) % m
  def posMod(a: Int, m: Int): Int = (a % m + m) % m

  def modularInverse(a: BigInt, m: BigInt): BigInt =
    val (_, x, _) = gcdExtended(a, m)
    (x % m + m) % m

  // Return gcd(a, b), x, y such that a*x + b*y = gcd(a, b)
  def gcdExtended(a: BigInt, b: BigInt): (BigInt, BigInt, BigInt) =
    if a == 0 then (b, 0, 1)
    else
      val (gcd, x1, y1) = gcdExtended(b % a, a)
      val x             = y1 - (b / a) * x1
      val y             = x1

      (gcd, x, y)

  def gcd(a: Int, b: Int): Int = if b == 0 then a.abs else gcd(b, a % b)

  def gcd(a: BigInt, b: BigInt): BigInt = if b == 0 then a.abs else gcd(b, a % b)

  def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(BigInt(1))((a, b) => (a / gcd(a, b)) * b)

  def ceilDiv(a: Long, b: Long): Long = (a + b - 1) / b

  case class NewtonQuadraticInterpolation(p0: Pos, p1: Pos, p2: Pos):
    private val x0 = p0.x
    private val x1 = p1.x
    private val x2 = p2.x
    private val y0 = p0.y
    private val y1 = p1.y
    private val y2 = p2.y
    private val b0 = y0
    private val b1 = BigDecimal(y1 - y0) / (x1 - x0)
    private val b2 = (BigDecimal(y2 - y1) / (x2 - x1) - b1) / (x2 - x0)

    def apply(x: BigInt): BigDecimal = b0 + b1 * BigDecimal(x - x0) + b2 * BigDecimal(x - x0) * BigDecimal(x - x1)

  /** Parse something like:
   * |-----|
   * |-c-d-|
   * |-*-d-|
   * |-*-d-|
   * |---d-|
   *
   * Bottom left corner is Pos(0, 0), so North is Up and East is to the right.
   */
  def mapParser[A](tileParser: Parser[A], ignore: Option[Parser[Any]]): Parser[Map[Vector.Pos, A]] =
    val ignoreParser: Parser[Any] = ignore.getOrElse(Parser.succeed(()))

    def getX(origSize: Int): Parser[Int] = for remainingLen <- Parser.peekRemainingLen
    yield origSize - remainingLen

    def parseLine(origSize: Int, y: Int): Parser[Map[Vector.Pos, A]] = getX(origSize).flatMap { x =>
      if x == origSize then Parser.succeed(Map.empty)
      else
        for
          aOrAny <- tileParser.orElseEither(ignoreParser)
          m      <- aOrAny match
                      case Left(a)  => parseLine(origSize, y).map(_.updated(Vector.Pos(x, y), a))
                      case Right(_) => parseLine(origSize, y)
        yield m
    }

    def lineParser(y: Int): Parser[Map[Vector.Pos, A]] = Parser.peekRemainingLen.flatMap(parseLine(_, y))

    val reverseLines: Parser[Unit] = (s: String) =>
      val lines = s.split("\n")
      Some(() -> lines.reverse.mkString("\n"))

    Parser
      .nonEmptyListOf("\n")(Parser.parseNonEmptyLine)
      .map(_.mkString("\n"))
      .feedInto(for
        _           <- reverseLines
        lines       <- Parser.nonEmptyListOf("\n")(Parser.parseLine).map(_.zipWithIndex)
        lineResults <- Parser.foreach(lines.map { case (line, y) => Parser.succeed(line).feedInto(lineParser(y)) })
      yield lineResults.foldLeft(Map.empty[Vector.Pos, A])(_ ++ _))
  end mapParser

  def visualizeMap[A](
    m: Map[Vector.Pos, A],
    showA: A => String = (a: A) => a.toString,
    emptySpace: String = " "
  ): String =
    val ps   = m.keySet
    val minY = ps.map(_.y).min
    val maxY = ps.map(_.y).max
    val minX = ps.map(_.x).min
    val maxX = ps.map(_.x).max
    (for
      y   <- maxY to minY by -1
      line = (minX to maxX).map(x => m.get(Pos(x, y)).map(showA).getOrElse(emptySpace)).mkString
    yield line).mkString("\n")

  def visualizeMapDoodle[A](
    m: Map[Vector.Pos, A],
    showA: A => Picture[Unit],
    emptySpace: Picture[Unit]
  ): Picture[Unit] =
    val ps   = m.keySet
    val minY = ps.map(_.y).min
    val maxY = ps.map(_.y).max
    val minX = ps.map(_.x).min
    val maxX = ps.map(_.x).max

    (for
      y  <- maxY to minY by -1
      row = (minX to maxX).map(x => m.get(Pos(x, y)).map(showA).getOrElse(emptySpace)).reduce(_ beside _)
    yield row).reduce(_ above _)

  def drawPictureUnsafe(pic: Picture[Unit]): Unit =
    import cats.effect.unsafe.implicits.global
    pic.draw()

  def normalizeNewLine(s: String): String = s.replace("\r\n", "\n")
end Util
