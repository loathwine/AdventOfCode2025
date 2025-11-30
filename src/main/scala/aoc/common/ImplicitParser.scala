package aoc.common

import zio.prelude.{NonEmptyList, NonEmptyMap}

import aoc.common.Parser
import aoc.common.data.PositiveInt
import scala.language.implicitConversions
import scala.util.Try

object ImplicitParser:
  def apply[A](using p: Parser[A]): Parser[A]          = p
  def parse[A](using p: Parser[A]): Parser[A]          = p
  def unsafeParse[A](s: String)(using p: Parser[A]): A = p.unsafeParse(s)

  given Parser[String]      = Parser.parseAnyString
  given Parser[Int]         = Parser.parseInt(10)
  given Parser[Char]        = Parser.parseAnyChar
  given Parser[PositiveInt] = Parser.parsePositiveIntTyped(10)
  given Parser[BigInt]      = Parser.parseBigInt(10)
  given Parser[Long]        = Parser.parseLong(10)
