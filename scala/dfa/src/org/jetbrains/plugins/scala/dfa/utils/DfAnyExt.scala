package org.jetbrains.plugins.scala.dfa
package utils

final class DfAnyExt(private val dfAny: DfAny) extends AnyVal {
  def nullability: Nullability = Nullability(dfAny)
  def toBoolLat: BoolLat = BoolLat(dfAny)
  def toBoolSemiLat: Option[BoolLat] = BoolSemiLat(dfAny)
}
