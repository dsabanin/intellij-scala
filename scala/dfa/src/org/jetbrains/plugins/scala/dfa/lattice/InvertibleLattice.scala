package org.jetbrains.plugins.scala.dfa.lattice

import org.jetbrains.plugins.scala.dfa.lattice.InvertibleLatticeOps.InvertibleLatticeExt

trait InvertibleLattice[L] extends SemiLattice[L] {
  def invert(element: L): L
}

trait InvertibleLatticeOps {
  @inline
  implicit final def invertibleLatticeExt[L](element: L): InvertibleLatticeExt[L] =
    new InvertibleLatticeExt(element)

  final def invert[L](element: L)(implicit lattice: InvertibleLattice[L]): L =
    lattice.invert(element)
}

object InvertibleLatticeOps extends InvertibleLatticeOps {
  final class InvertibleLatticeExt[L](private val element: L) extends AnyVal {
    def inverted(implicit lattice: InvertibleLattice[L]): L =
      lattice.invert(element)
  }
}