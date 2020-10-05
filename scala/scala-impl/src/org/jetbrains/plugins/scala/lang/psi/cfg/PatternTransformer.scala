package org.jetbrains.plugins.scala.lang.psi.cfg

import org.jetbrains.plugins.scala.dfa.DfNothing
import org.jetbrains.plugins.scala.lang.psi.api.base.ScPatternList
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScParenthesisedPattern, ScPattern, ScReferencePattern}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression

import scala.annotation.tailrec

private trait PatternTransformer { this: Transformer =>
  final def transformPatternList(patternList: ScPatternList, expr: Option[ScExpression]): Unit = {
    val value = expr.fold(builder.constant(DfNothing))(expr => transformExpression(expr))

    for (pattern <- patternList.patterns) {
      transformPattern(pattern, value)
    }
  }

  @tailrec
  final def transformPattern(pattern: ScPattern, value: builder.Value): Unit = pattern match {
    case ScParenthesisedPattern(pattern) => transformPattern(pattern, value)
    case pattern: ScReferencePattern => builder.writeVariable(variable(pattern), value)
    case _ => transformationNotSupported
  }
}
