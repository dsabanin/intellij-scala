package org.jetbrains.plugins.scala.dfa
package utils

final class DfAnyExt(private val dfAny: DfAny) extends AnyVal {
  def canTheoreticallyBeNull: Boolean = dfAny.narrow[DfNull] != DfNull.Bottom
  def canLikelyBeNull: Boolean = dfAny.narrow[DfNull] == DfNull.Top
}
