package aoc.common

import zio.prelude.NonEmptyList

import breeze.linalg.DenseVector

object VectorUtils:

  case class Vec(values: NonEmptyList[BigDecimal]):
    def x: BigDecimal                 = values(0)
    def y: BigDecimal                 = values(1)
    def z: BigDecimal                 = values(2)
    def -(other: Vec): Vec            = Vec((values zip other.values).map { case (a, b) => a - b })
    def +(other: Vec): Vec            = Vec((values zip other.values).map { case (a, b) => a + b })
    def *(scalar: BigDecimal): Vec    = Vec(values.map(_ * scalar))
    def dot(other: Vec): BigDecimal   = (values zip other.values).map { case (a, b) => a * b }.sum
    def asBreeze: DenseVector[Double] = DenseVector(values.map(_.toDouble).toArray)

  case class Particle(position: Vec, velocity: Vec):
    def positionAt(time: BigDecimal): Vec = position + velocity * time

  case class IntersectionPoint(at: Vec, t1: BigDecimal, t2: BigDecimal)

  def intersectionPointOfLines(p1: Particle, p2: Particle): Option[IntersectionPoint] =
    // Only implemented for 2d for now
    val eps           = BigDecimal("0.0000000000000000000001")
    val vecDimensions = p1.position.values.size
    if vecDimensions == 2 then
      // https://stackoverflow.com/questions/2931573/determining-if-two-rays-intersect
      val as = p1.position
      val ad = p1.velocity
      val bs = p2.position
      val bd = p2.velocity

      val dx  = bs.x - as.x
      val dy  = bs.y - as.y
      val det = bd.x * ad.y - bd.y * ad.x
      Option.when(det.abs > eps) {
        val u = (dy * bd.x - dx * bd.y) / det
        val v = (dy * ad.x - dx * ad.y) / det
        IntersectionPoint(p1.positionAt(u), u, v)
      }

      /*
      /* Try to solve this equation, see https://stackoverflow.com/a/73079842
      |v_x_1, -v_x_2| * |s| = |p_x_2 - p_x_1|
      |v_y_1, -v_y_2|   |t|   |p_y_2 - p_y_1|

      The determinant is v_x_1 * -v_y_2 + v_x_2 * v_y_1
       */
      val a = p1.velocity.x
      val b = -p2.velocity.x
      val c = p1.velocity.y
      val d = -p2.velocity.y

      val determinant = a * d - b * c
      val isOk        = determinant.abs > eps
      println(isOk)
      Option.when(determinant.abs > eps) {
        // [s, t] = 1/determinant * [(p_x_1 - p_x_2), (p_y_1 - p_y_2)]
        val timesSandT = Vec(
          NonEmptyList(
            p1.position.values.head - p2.position.values.head,
            p1.position.values.tail.head - p2.position.values.tail.head
          )
        ) * (BigDecimal(1) / determinant)
        // t1 = (v2_y * (p2_x - p1_x) - v2_x * (p2_y - p1_y)) / determinant
        val t1         =
          (p2.velocity.y * (p2.position.x - p1.position.x) - p2.velocity.x * (p2.position.y - p1.position.y)) / determinant
        // (v1_x * (p2_y - p1_y) - v1_y * (p2_x - p1_x)) / determinant
        val t2         =
          (p1.velocity.x * (p2.position.y - p1.position.y) - p1.velocity.y * (p2.position.x - p1.position.x)) / determinant
        val s          = timesSandT.x
        val t          = timesSandT.y
        IntersectionPoint(p1.positionAt(t), t, s)
      }
    } else throw new IllegalArgumentException(s"$vecDimensions not supported (yet!)")
       */

      /*
    val relativePosition = p1.position - p2.position
    val relativeVelocity = p1.velocity - p2.velocity
    if (relativeVelocity.values.forall(_ == 0)) {
      Option.when(relativePosition.values.forall(_ == 0))(IntersectionPoint(p1.position, 0, 0))
    } else {
      val t                 = -(relativePosition dot relativeVelocity) / (relativeVelocity dot relativeVelocity)
      val intersectionPoint = p1.positionAt(t)
      val t1                = (intersectionPoint - p1.position) dot p1.velocity
      val t2                = (intersectionPoint - p2.position) dot p2.velocity
      Some(IntersectionPoint(intersectionPoint, t1, t2))
    }
       */
    else throw new IllegalArgumentException(s"$vecDimensions not supported (yet!)")
    end if
  end intersectionPointOfLines
end VectorUtils
