package aoc.common.data

import neotype.Subtype

type PositiveInt = PositiveInt.Type
object PositiveInt extends Subtype[Int]:
  override inline def validate(input: Int): Boolean | String = if input > 0 then true else "Not positive int"
