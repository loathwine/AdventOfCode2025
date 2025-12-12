package aoc.common

import zio.prelude.{NonEmptyList, NonEmptyMap}

import aoc.common.Parser
import aoc.common.data.PositiveInt
import scala.language.implicitConversions
import scala.util.Try

// Inspired by "Declarative vs Executable Encodings" https://www.youtube.com/watch?v=OD1Yr48-0Js&ab_channel=Ziverge
// Using executable encoding
trait Parser[+A]:
  self =>
  def parse(s: String): Option[(A, String)]

  def unsafeParse(s: String): A = parse(s)
    .filter(_._2.forall(_.isWhitespace))
    .getOrElse(throw new IllegalArgumentException("Could not parse input"))
    ._1

  def flatMap[B](f: A => Parser[B]): Parser[B] = (s: String) =>
    self.parse(s) match
      case Some((a, remainder)) =>
        f(a).parse(remainder)
      case None                 => None

  def map[B](f: A => B): Parser[B] = flatMap(a => Parser.succeed(f(a)))

  def withFilter(f: A => Boolean): Parser[A] = (s: String) =>
    self.parse(s) match
      case Some((a, remainder)) => Option.when(f(a))(a -> remainder)
      case None                 => None

  def feedInto[B](p: Parser[B])(implicit ev: A <:< String): Parser[B] = (s: String) =>
    self.parse(s).flatMap { case (a, remainder) =>
      p.parse(a).map { case (b, _) => b -> remainder }
    }

  def *>[B](other: Parser[B]): Parser[B] = flatMap(_ => other)

  def <*[B](other: Parser[B]): Parser[A] = flatMap(a => other.as(a))

  def <*>[B](other: Parser[B]): Parser[(A, B)] = flatMap(a => other.map(a -> _))

  infix def or[B](other: Parser[B]): Parser[A | B] = (s: String) =>
    self.parse(s) match
      case None    => other.parse(s)
      case success => success

  infix def orElse[B >: A](other: Parser[B]): Parser[B] = (s: String) =>
    self.parse(s) match
      case None    => other.parse(s)
      case success => success

  infix def orElseEither[B](other: Parser[B]): Parser[Either[A, B]] = (s: String) =>
    self.map(Left(_)).parse(s) match
      case None    => other.map(Right(_)).parse(s)
      case success => success

  def as[B](b: => B): Parser[B] = flatMap(_ => Parser.succeed(b))

  def option: Parser[Option[A]] = (s: String) =>
    self.parse(s) match
      case Some((a, remainder)) => Some(Some(a) -> remainder)
      case None                 => Some(None -> s)

  def multiple(n: Int): Parser[List[A]] = Parser.multiple(n, Some(n), None)(self)
end Parser

object Parser:
  def succeed[A](a: A): Parser[A] = (s: String) => Some(a -> s)

  def fail: Parser[Nothing] = (_: String) => None

  def fromOption[A](o: Option[A]): Parser[A] = o.fold(fail)(succeed)

  def peekRemainingLen: Parser[Int] = (s: String) => Some(s.length -> s)

  def peek: Parser[String] = (s: String) => Some(s -> s)

  def when[A](cond: => Boolean)(p: => Parser[A]): Parser[Option[A]] =
    if cond then p.map(Some(_)) else Parser.succeed(None)

  def multiple[A](min: Int, max: Option[Int], separator: Option[Parser[Any]])(p: Parser[A]): Parser[List[A]] =
    (input: String) =>
      val sepParser              = separator match
        case Some(sep) => sep *> p
        case None      => p
      val successfulParseResults = LazyList
        .iterate(p.parse(input))(_.flatMap { case (_, rest) => sepParser.parse(rest) })
        .zipWithIndex
        .takeWhile { case (res, idx) => res.isDefined && max.forall(_ > idx) }
        .map(_._1.get)
        .toList
      Option.when(successfulParseResults.size >= min) {
        val lastRemainder = successfulParseResults.lastOption.map(_._2).getOrElse(input)
        successfulParseResults.map(_._1) -> lastRemainder
      }

  def multipleWithStringSep[A](min: Int, max: Option[Int], separator: Option[String])(p: Parser[A]): Parser[List[A]] =
    multiple(min, max, separator.map(eatString))(p)

  def listOf[A]: Parser[A] => Parser[List[A]] = multipleWithStringSep(0, None, None)

  def atLeast[A](n: Int): Parser[A] => Parser[List[A]] = multipleWithStringSep(n, None, None)

  def atMost[A](n: Int): Parser[A] => Parser[List[A]] = multipleWithStringSep(0, Some(n), None)

  def exactly[A](n: Int): Parser[A] => Parser[List[A]] = multipleWithStringSep(n, Some(n), None)

  def separatedBy[A](separator: String): Parser[A] => Parser[List[A]] = multipleWithStringSep(0, None, Some(separator))

  def nonEmptyListOf[A](separator: String): Parser[A] => Parser[NonEmptyList[A]] =
    val parseList  = multipleWithStringSep[A](1, None, Some(separator))
    val toNonEmpty = (p: Parser[List[A]]) => p.map(list => NonEmptyList.fromIterableOption(list).get)
    parseList andThen toNonEmpty

  def foreach[A](parsers: List[Parser[A]]): Parser[List[A]] = parsers match
    case Nil          => succeed(Nil)
    case head :: tail => head.flatMap(a => foreach(tail).map(a :: _))

  def filter[A](p: Parser[A]): Parser[List[A]] =
    listOf(p.orElseEither(parseAnyChar)).map(_.collect { case Left(a) =>
      a
    })

  def parseChar(c: Char): Parser[Char] = (s: String) => s.headOption.filter(_ == c).map(_ -> s.tail)

  val parseAnyChar: Parser[Char] = (s: String) => s.headOption.map(_ -> s.tail)

  def charMap[A](mapping: NonEmptyMap[Char, A]): Parser[A] =
    mapping.map { case (c, a) => Parser.parseChar(c).as(a) }.reduce(_ orElse _)

  def charMap[A](elem: (Char, A), others: (Char, A)*): Parser[A] = charMap(NonEmptyMap(elem, others))

  def eatString(yummy: String): Parser[Unit] = for
    eaten <- parseStringOfLen(yummy.length)
    if eaten == yummy
  yield ()

  def parseString(s: String): Parser[String] = for
    c <- parseStringOfLen(s.length)
    if c == s
  yield c

  val parseAnyString: Parser[String] = (s: String) => Some(s -> "")

  def eatNewLine: Parser[Unit] = eatString("\n") orElse eatString("\r\n")

  val parseWord: Parser[String] = (s: String) =>
    val word = s.takeWhile(!_.isWhitespace)
    Option.when(word.nonEmpty)(word -> s.drop(word.length))

  val parseLine: Parser[String] = (s: String) =>
    val line = s.takeWhile(_ != '\n')
    Some(line -> s.drop(line.length))

  val parseNonEmptyLine: Parser[String] = (s: String) =>
    val line = s.takeWhile(_ != '\n')
    Option.when(line.nonEmpty)(line -> s.drop(line.length))

  val parseAlphaWord: Parser[String] = (s: String) =>
    val word = s.takeWhile(_.isLetter)
    Option.when(word.nonEmpty)(word -> s.drop(word.length))

  val eatSpaces: Parser[Unit] = (s: String) =>
    val whitespace = s.takeWhile(_.isSpaceChar)
    Option.when(whitespace.nonEmpty)(() -> s.drop(whitespace.length))

  def parseStringOfLen(len: Int): Parser[String] = (s: String) =>
    if s.size >= len then Some(s.take(len) -> s.drop(len))
    else None

  def parsePositiveInt(base: Int): Parser[BigInt] = (s: String) =>
    val validChars: Set[Char] =
      (0 until base).map(i => if i < 10 then ('0' + i).toChar else ('a' + (i - 10)).toChar).toSet
    val digits                = s.takeWhile(validChars.contains)
    Try(BigInt(digits, base)).toOption.map(i => i -> s.drop(digits.length))

  def parsePositiveIntTyped(base: Int): Parser[PositiveInt] =
    parsePositiveInt(base).flatMap(bi =>
      Option.when(bi.isValidInt)(PositiveInt.make(bi.toInt).toOption).flatten match
        case Some(pi) => succeed(pi)
        case None     => fail
    )

  def parseBigInt(base: Int): Parser[BigInt] =
    (eatString("-") *> parsePositiveInt(base).map(-_)) orElse parsePositiveInt(base)

  def parseInt(base: Int): Parser[Int] =
    parseBigInt(base).flatMap(bi => if bi.isValidInt then Parser.succeed(bi.toInt) else Parser.fail)

  val parseDigit: Parser[Int] = parseAnyChar.flatMap(c => if c.isDigit then Parser.succeed(c.asDigit) else Parser.fail)

  def parseLong(base: Int): Parser[Long] =
    parseBigInt(base).flatMap(bi => if bi.isValidLong then Parser.succeed(bi.toLong) else Parser.fail)

  def parseIntOfLen(len: Int, base: Int): Parser[Int] = parseStringOfLen(len).feedInto(parseInt(base))

  def parseIntOfLen(len: Int): Parser[Int] = parseIntOfLen(len, 10)

  implicit def stringToParser(s: String): Parser[Unit] = eatString(s)
end Parser
