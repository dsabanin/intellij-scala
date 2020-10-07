package org.jetbrains.plugins.scala.dfa
package lattice

trait InvertibleLatticeSpec[L] extends SemiLatticeSpec[L] {
  override protected def lattice: InvertibleLattice[L]

  private implicit lazy val _lattice: InvertibleLattice[L] = lattice

  latticeHasTop.foreach { implicit hasTop =>
    latticeHasBottom.foreach { implicit hasBottom =>
      property("X == ¬X => (X == Top | X == Bottom)") {
        forAll { (x: L) =>
          val inverted = x.inverted
          whenever(x == inverted) {
            x should (be (latticeTop) or be (latticeBottom))
          }
        }
      }
    }
  }

  property("X != ¬X => !(X intersects ¬X)") {
    forAll { (x: L) =>
      val inverted = x.inverted
      whenever(x != inverted) {
        assert(!(x intersects inverted))
      }
    }
  }

  property("¬¬X intersects X") {
    forAll { (x: L) =>
      val inverted = x.inverted
      whenever(x != inverted) {
        assert(!(x intersects inverted))
      }
    }
  }
}
