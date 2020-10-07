package org.jetbrains.plugins.scala.dfa

import org.jetbrains.plugins.scala.dfa.lattice.JoinSemiLattice

/**
 * {{{
 *         MaybeNull
 *          /   \
 *         / MaybeNullButNotExpected
 *        /      |
 *  AlwaysNull  NeverNull
 * }}}
 *
 * Lattice that describes Nullability.
 * We can either be sure that it is always or never null.
 * But we can also be unsure and it could be either way.
 *
 * Additionally we can track if the value can be null but is usually should not.
 * This is needed because returns or parameters in Scala are not null
 * by convention, but they still can be null.
 *
 * {{{
 *   def test: Null    // -> AlwaysNull
 *
 *   @NotNull
 *   def test: String  // -> NeverNull
 *
 *   @Nullable
 *   def test: String  // -> MaybeNull
 *
 *   def test: String  // -> NotExpected
 * }}}
 *
 */
sealed abstract class Nullability(val canBeNullButIsNotExpected: Boolean,
                                  val canBeNotNull: Boolean,
                                  val canBeNullAndIsExpected: Boolean)

object Nullability {
  final case object MaybeNull extends Nullability(
    canBeNullButIsNotExpected = true,
    canBeNotNull = true,
    canBeNullAndIsExpected = true
  )

  final val Top: MaybeNull.type = MaybeNull

  final case object AlwaysNull extends Nullability(
    canBeNullButIsNotExpected = true,
    canBeNotNull = false,
    canBeNullAndIsExpected = true
  )

  final case object MaybeNullButNotExpected extends Nullability(
    canBeNullButIsNotExpected = true,
    canBeNotNull = true,
    canBeNullAndIsExpected = false
  )

  final case object NeverNull extends Nullability(
    canBeNullButIsNotExpected = false,
    canBeNotNull = true,
    canBeNullAndIsExpected = false
  )

  def apply(dfAny: DfAny): Nullability = dfAny match {
    case DfNull.Always => Nullability.AlwaysNull
    case _ => dfAny.narrowed[DfNull] match {
      case DfNull.Always => Nullability.MaybeNull
      case DfNull.Unexpected => Nullability.MaybeNullButNotExpected
      case DfNull.Bottom => Nullability.NeverNull
    }
  }

  implicit val semiLattice: JoinSemiLattice[Nullability] = new JoinSemiLattice[Nullability] {
    override def top: Nullability = Top

    override def <=(subSet: Nullability, superSet: Nullability): Boolean =
      subSet.canBeNullButIsNotExpected <= superSet.canBeNullButIsNotExpected &&
        subSet.canBeNotNull <= superSet.canBeNotNull &&
        subSet.canBeNullAndIsExpected <= superSet.canBeNullAndIsExpected

    override def intersects(lhs: Nullability, rhs: Nullability): Boolean =
      lhs.canBeNullButIsNotExpected == rhs.canBeNullButIsNotExpected || lhs.canBeNotNull == rhs.canBeNotNull

    override def join(lhs: Nullability, rhs: Nullability): Nullability =
      if (lhs == rhs) lhs
      else if (lhs.canBeNullAndIsExpected || rhs.canBeNullAndIsExpected) MaybeNull
      else MaybeNullButNotExpected
  }
}
