package org.jetbrains.plugins.scala.dfa

import org.jetbrains.plugins.scala.dfa.lattice.specific.{FlatJoinSemiLattice, FlatLattice}
import org.jetbrains.plugins.scala.dfa.lattice.{InvertibleLattice, JoinSemiLattice, Lattice}

/**
 * A complete bool lattice
 *
 *       Top
 *     /    \
 *    /      \
 *  True    False
 *    \      /
 *     \    /
 *     Bottom
 */
sealed abstract class BoolLat(final val canBeTrue: Boolean, final val canBeFalse: Boolean)
  extends Product with Serializable
{
  final val isConcrete: Boolean = canBeTrue != canBeFalse

  final def asConcrete: Option[Boolean] =
    if (isConcrete) Some(canBeTrue) else None
}

object BoolLat {
  def apply(boolean: Boolean): Concrete = Concrete(boolean)

  def apply(boolean: Option[Boolean]): BoolLat =
    boolean.fold(Bottom: BoolLat)(BoolLat(_))

  def apply(dfAny: DfAny): BoolLat = dfAny.narrowed[DfBool] match {
    case DfBool.Top => BoolLat.Top
    case DfBool.True => BoolLat.True
    case DfBool.False => BoolLat.False
    case DfBool.Bottom => BoolLat.Bottom
  }

  sealed trait Concrete extends BoolSemiLat

  object Concrete {
    def apply(boolean: Boolean): Concrete =
      if (boolean) True else False
    def unapply(boolean: Concrete): Some[Boolean] = Some(boolean.canBeTrue)
  }

  /**
   * Describes a value that can be true as well as false
   */
  final case object Top extends BoolSemiLat(
    canBeTrue = true,
    canBeFalse = true,
  )

  final type Maybe = Top.type
  final val Maybe: Maybe = Top

  final case object True extends BoolSemiLat(
    canBeTrue = true,
    canBeFalse = false,
  ) with Concrete

  final case object False extends BoolSemiLat(
    canBeTrue = false,
    canBeFalse = true,
  ) with Concrete

  /**
   * Describes a value that can neither be true nor false
   */
  final case object Bottom extends BoolLat(
    canBeTrue = false,
    canBeFalse = false,
  )

  implicit val lattice: Lattice[BoolLat] with InvertibleLattice[BoolLat] =
    new FlatLattice[BoolLat](Top, Bottom) with InvertibleLattice[BoolLat] {
      final override def invert(element: BoolLat): BoolLat = element match {
        case Concrete(bool) => BoolSemiLat(!bool)
        case self => self
      }
    }
}

/**
 * A join-semi-lattice for boolean that does not have a Bottom element
 *
 * {{{
 *       Top
 *     /    \
 *    /      \
 *  True    False
 * }}}
 */
sealed abstract class BoolSemiLat(canBeTrue: Boolean, canBeFalse: Boolean) extends BoolLat(canBeTrue, canBeFalse)

object BoolSemiLat {
  val Top: BoolLat.Top.type = BoolLat.Top
  val True: BoolLat.True.type = BoolLat.True
  val False: BoolLat.False.type = BoolLat.False

  val Concrete: BoolLat.Concrete.type = BoolLat.Concrete

  def apply(boolean: Boolean): BoolSemiLat =
    if (boolean) True else False

  def apply(boolLat: BoolLat): Option[BoolSemiLat] = boolLat match {
    case BoolLat.Bottom => None
    case semi: BoolSemiLat => Some(semi)
  }

  def apply(dfAny: DfAny): Option[BoolLat] =
    BoolSemiLat(dfAny.toBoolLat)

  implicit val joinSemiLattice: JoinSemiLattice[BoolSemiLat] with InvertibleLattice[BoolSemiLat] =
    new FlatJoinSemiLattice[BoolSemiLat](Top) with InvertibleLattice[BoolSemiLat] {
      final override def invert(element: BoolSemiLat): BoolSemiLat = element match {
        case Concrete(bool) => BoolSemiLat(!bool)
        case self => self
      }
    }
}