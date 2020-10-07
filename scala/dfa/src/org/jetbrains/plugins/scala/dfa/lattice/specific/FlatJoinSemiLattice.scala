package org.jetbrains.plugins.scala.dfa.lattice
package specific

/**
 * See [[FlatLattice]]
 */
class FlatJoinSemiLattice[T](override val top: T)
  extends JoinSemiLattice[T]
{
  final override def <=(subSet: T, superSet: T): Boolean = (subSet, superSet) match {
    case (a, b) if a == b => true
    case (_, `top`) => true
    case _ => false
  }

  final override def intersects(lhs: T, rhs: T): Boolean = (lhs, rhs) match {
    case (a, b) if a == b => true
    case (`top`, _) => true
    case (_, `top`) => true
    case _ => false
  }

  final override def join(lhs: T, rhs: T): T =
    if (lhs == rhs) lhs else top

  final override def joinAll(first: T, others: IterableOnce[T]): T = {
    if (first == top) top
    else {
      val allTheSame = others.iterator.forall(_ == first)
      if (allTheSame) first
      else top
    }
  }
}
