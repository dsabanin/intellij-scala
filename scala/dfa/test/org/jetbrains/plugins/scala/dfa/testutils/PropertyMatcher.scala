package org.jetbrains.plugins.scala.dfa.testutils

import org.scalatest.matchers.{MatchResult, Matcher}

object PropertyMatcher {
  def apply[T](propertyIsTrueOn: T => Boolean, propertyName: String): Matcher[T] =
    (left: T) => new MatchResult(
      propertyIsTrueOn(left),
      s"$left.$propertyName was not true",
      s"$left.$propertyName was not false"
    )
}
