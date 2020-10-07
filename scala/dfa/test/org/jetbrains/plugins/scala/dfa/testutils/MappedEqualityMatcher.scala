package org.jetbrains.plugins.scala.dfa.testutils

import org.scalactic.DefaultEquality.areEqualComparingArraysStructurally
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

object MappedEqualityMatcher extends Matchers{
  def apply[L, R](map: L => R, buildSubject: L => String): R => Matcher[L] =
    (right: R) => (left: L) => {
      val mappedLeft = map(left)
      val subject = buildSubject(left)
      new MatchResult(
        areEqualComparingArraysStructurally(mappedLeft, right),
        s"$subject was expected to be $right, but was $mappedLeft",
        s"$subject was expected to not be $right",
      )
    }
}
