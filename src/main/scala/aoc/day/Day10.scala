package aoc.day

import zio.*

import aoc.common.ImplicitParser.{*, given}
import aoc.common.Input.{InputString, ResourceInput}
import aoc.common.Parser
import aoc.day.DayCase.{Puzzle, Test}
import neotype.unwrap

object Day10 extends Day:
  type P1 = Int
  type P2 = Int
  import neotype.Newtype

  type Lights = Lights.Type
  object Lights extends Newtype[Int]:
    val AllOff: Lights = Lights(0)
    extension (l: Lights)
      def toggle(lamps: Lights): Lights = Lights(
        l.unwrap ^ lamps.unwrap
      )
      def toList: List[Int]             = (0 to 31).flatMap(i => Option.when(((1 << i) & l.unwrap) != 0)(i)).toList

  case class Machine(lights: Lights, buttons: List[Lights], joltage: List[Int])

  object Machine:
    val parseIndicatorLights      = Parser.eatString("[") *> Parser
      .multiple(1, None, None)(Parser.charMap('.' -> 0, '#' -> 1))
      .map(_.reverse.mkString) // Reverse to put least significant lamp or the right side
      .feedInto(Parser.parseInt(2))
      .map(Lights.apply) <* "]"
    val parseButton               = (Parser.eatString("(") *> Parser.separatedBy(",")(Parser.parseInt(10)) <* ")")
      .map(lamps => lamps.foldLeft(0)((acc, lamp) => acc | (1 << lamp)))
      .map(Lights.apply)
    given parser: Parser[Machine] =
      for
        lights  <- parseIndicatorLights <* " "
        buttons <- Parser.separatedBy(" ")(parseButton) <* " "
        joltage <- Parser.eatString("{") *> Parser.separatedBy(",")(Parser.parseInt(10)) <* "}"
      yield Machine(lights, buttons, joltage)

  // I'm assuming each button needs only be pressed at most once. Pressing it twice doesn't make sense, you could have just not pressed it.
  def minButtonPresses(target: Lights, current: Lights, buttons: List[Lights]): Option[Int] =
    buttons match
      case Nil          => Option.when(current == target)(0)
      case head :: tail =>
        // Either we press button or not
        val pressed    = minButtonPresses(target, current.toggle(head), tail).map(_ + 1)
        val notPressed = minButtonPresses(target, current, tail)
        List(pressed, notPressed).flatten.minOption
  end minButtonPresses

  case class Prob2(machine: Machine):
    type RemainingButtons = Int
    type JoltageCounters  = Vector[Int]
    var memo: Map[(JoltageCounters, RemainingButtons), Option[Int]] = Map.empty

    // Try dynamic programming. Set target to 0 and instead lower joltage by one, so target is all zeroes.
    // Update: Not feasible for puzzle input. Great for heating up CPU! Works on example.
    def minButtonPressesToReachJoltage(current: Vector[Int], buttons: List[List[Int]]): Option[Int] =
      val key = current -> buttons.size
      if memo.contains(key) then memo(key)
      else
        val res = buttons match
          case Nil              => Option.when(current.forall(_ == 0))(0)
          case affected :: tail =>
            val maxPresses   = affected.map(current.apply).max
            val alternatives = (0 to maxPresses).flatMap(presses =>
              minButtonPressesToReachJoltage(affected.foldLeft(current)((v, i) => v.updated(i, v(i) - presses)), tail)
                .map(_ + presses)
            )
            alternatives.minOption
        memo = memo.updated(key, res)
        res

    def solveUnsafe: Int =
      val res = minButtonPressesToReachJoltage(machine.joltage.toVector, machine.buttons.map(_.toList)).get
      println(s"Solved $machine with $res")
      res
  end Prob2

  def part1(in: String): Task[P1] = ZIO.attempt {
    val machines = Parser.separatedBy("\n")(Machine.parser).unsafeParse(in)
    machines.map(m => minButtonPresses(m.lights, Lights.AllOff, m.buttons).get).sum
  }

  def part2(in: String): Task[P2] = ZIO.attempt {
    val machines = Parser.separatedBy("\n")(Machine.parser).unsafeParse(in)
//    machines.map(Prob2.apply).map(_.solveUnsafe).sum
    ???
  }

  val cases: List[DayCase[P1, P2]] = List(
    Test(
      "example",
      InputString("""[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
                    |[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
                    |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}""".stripMargin),
      7,
      33
    ),
    Puzzle(ResourceInput("day10puzzle.txt"), 415)
  )
end Day10
