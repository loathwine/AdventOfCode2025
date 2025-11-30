package aoc.common

// The "Image" DSL is the easiest way to create images
import aoc.common.Util
// Colors and other useful stuff
import doodle.core.*
import doodle.effect.Renderer
import doodle.image.*
import doodle.java2d.*
import doodle.syntax.all.*

object DoodleExperiment extends App:
  val blackSquare = Picture.rectangle(30, 30).fillColor(Color.black)
  val redSquare   = Picture.rectangle(30, 30).fillColor(Color.red)

  // A chessboard, broken into steps showing the recursive construction
  val twoByTwo =
    redSquare
      .beside(blackSquare)
      .above(blackSquare.beside(redSquare))

  val fourByFour =
    twoByTwo
      .beside(twoByTwo)
      .above(twoByTwo.beside(twoByTwo))

  val chessboard =
    fourByFour
      .beside(fourByFour)
      .above(fourByFour.beside(fourByFour))

  // Need the Cats Effect runtime to run everything
  Util.drawPictureUnsafe(chessboard)
  Util.drawPictureUnsafe(chessboard.beside(chessboard))
end DoodleExperiment
