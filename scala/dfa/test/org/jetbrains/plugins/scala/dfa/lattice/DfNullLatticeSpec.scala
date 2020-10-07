package org.jetbrains.plugins.scala.dfa
package lattice

class DfNullLatticeSpec extends LatticeSpec[DfNull] {
  override protected def lattice: Lattice[DfNull] = DfNull.lattice

  override protected def latticeElementSamples: Seq[DfNull] =
    DfNullLatticeSpec.latticeElementSamples
}

object DfNullLatticeSpec {
  val latticeElementSamples: Seq[DfNull] =
    Seq(DfNull.Top, DfNull.Unexpected, DfNull.Bottom)
}