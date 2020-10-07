package org.jetbrains.plugins.scala.dfa

import org.jetbrains.plugins.scala.dfa.lattice.{JoinSemiLattice, JoinSemiLatticeSpec}
import org.jetbrains.plugins.scala.dfa.testutils.CustomMatchers._
import org.scalatest.prop.TableFor3

/**
 *          Maybe
 *          /   \
 *         /  UnexpectedNull
 *        /      |
 *     Always  Never
 *
 * Nullability can track if there is the possibility that a value is coming from
 * a place which can return null, but should not.
 *
 * The Nullability lattice is the Cartesian product of the normal Null semi lattice (Maybe, Always, Never)
 * and a lattice that tracks if the unexpected null can occur. Here we call that Nil for now.
 * However, the Cartesian product has a constraint, which limits the number of resulting elements in the lattice set.
 *
 * Here a breakdown using predicate logic.
 *
 *  Lattice: Null
 *   Null(X) = X can be null
 *   NotNull(X) = X can be something other than null
 *   NeverNull  = !Null(X) && NotNull(X)
 *   AlwaysNull = Null(X) && NotNull(X)
 *   MaybeNull  = Null(X) || NotNull(X)
 *
 *  Lattice: Nil
 *   Nil(X) = X can be nil
 *   NeverNil  = !Nil(X)
 *   AlwaysNil = Nil(X)
 *
 *  Lattice: Nullability = Null x Nil
 *  With constraints:
 *   c1: Null(X)   => Nil(X)
 *
 *  Here, now, the lattice elements and their cartesian deconstruction
 *  with the proofs that because of the constraint, one element cannot exist,
 *  and another es equivalent to another, resulting in 4 elements in the lattice set.
 *  (X argument is omitted to make it less cluttered)
 *
 *  (NeverNull,  NeverNil)  = Nullability.NeverNull
 *  (NeverNull,  AlwaysNil) = Nullability.MaybeNullButNotExpected
 *  (AlwaysNull, NeverNil)  cannot exist [AlwaysNull => Null =c1=>↯ !Nil = NeverNil]
 *  (AlwaysNull, AlwaysNil) = Nullability.AlwaysNull
 *  (MaybeNull,  NeverNil)  = Nullability.NeverNull  [(if Null: =c1=> Nil↯) => !Null && NotNull = NeverNull]
 *  (MaybeNull,  AlwaysNil) = Nullability.MaybeNull
 */
class NullabilitySpec extends JoinSemiLatticeSpec[Nullability] {
  override protected lazy val lattice: JoinSemiLattice[Nullability] = Nullability.semiLattice

  override protected def latticeHasBottom: None.type = None

  override protected lazy val latticeElementSamples: Seq[Nullability] =
    Seq(Nullability.NeverNull, Nullability.AlwaysNull, Nullability.MaybeNullButNotExpected, Nullability.MaybeNull)

  override protected lazy val latticeJoinSamples: TableFor3[Nullability, Nullability, Nullability] =
    Table(
      ("A", "B", "A join B"),
      (Nullability.NeverNull,               Nullability.MaybeNull,               Nullability.MaybeNull),
      (Nullability.AlwaysNull,              Nullability.MaybeNull,               Nullability.MaybeNull),
      (Nullability.MaybeNullButNotExpected, Nullability.MaybeNull,               Nullability.MaybeNull),

      (Nullability.NeverNull,               Nullability.AlwaysNull,              Nullability.MaybeNull),
      (Nullability.NeverNull,               Nullability.MaybeNullButNotExpected, Nullability.MaybeNullButNotExpected),
      (Nullability.AlwaysNull,              Nullability.MaybeNullButNotExpected, Nullability.MaybeNull),
    )

  property("can be constructed from DfNull") {
    DfNull.Always     should haveNullability (Nullability.AlwaysNull)
    DfNull.Unexpected should haveNullability (Nullability.MaybeNullButNotExpected)
    DfNothing         should haveNullability (Nullability.NeverNull)

    DfAny.Top should haveNullability(Nullability.MaybeNull)
    DfAnyVal.Top should haveNullability(Nullability.NeverNull)
  }
}
