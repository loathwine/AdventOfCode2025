package aoc.common

import zio.{Scope, ZIO}

import java.io.IOException

import aoc.common.Input
import aoc.common.Input.{InputString, ResourceInput}
import scala.io.Source

sealed trait Input:

  def getInput: ZIO[Scope, IOException, String] = this match
    case InputString(value)  => ZIO.succeed(value)
    case ResourceInput(path) =>
      ZIO
        .acquireRelease(ZIO.attempt(scala.io.Source.fromFile(s"src/main/resources/$path")))(s =>
          ZIO.attempt(s.close()).orDie
        )
        .map(_.getLines().mkString("\n"))
        .refineToOrDie[IOException]

object Input:
  case class InputString(value: String) extends Input

  case class ResourceInput(value: String) extends Input
