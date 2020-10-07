package org.jetbrains.plugins.scala.dfa.testutils

import org.jetbrains.plugins.scala.dfa.{DfAny, Nullability}
import org.scalatest.matchers.Matcher

trait CustomMatchers {
  val haveNullability: Nullability => Matcher[DfAny] =
    MappedEqualityMatcher[DfAny, Nullability](Nullability.apply, any => s"Nullability of $any")
}

object CustomMatchers extends CustomMatchers
