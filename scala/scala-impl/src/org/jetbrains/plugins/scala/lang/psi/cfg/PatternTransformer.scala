package org.jetbrains.plugins.scala.lang.psi.cfg

import org.jetbrains.plugins.scala.dfa.DfNothing
import org.jetbrains.plugins.scala.lang.psi.api.base.ScPatternList
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScParenthesisedPattern, ScPattern, ScReferencePattern}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression

import scala.annotation.tailrec

private trait PatternTransformer { this: Transformer =>
  final def transformPatternList(patternList: ScPatternList, expr: Option[ScExpression]): Unit = {
    val subject = expr.fold(builder.constant(DfNothing))(expr => transformExpression(expr))

    for (pattern <- patternList.patterns) {
      transformPattern(pattern, subject)
    }
  }

  final def transformPattern(pattern: ScPattern, subject: builder.Value): Unit =
    transformPatternWithCustomFail(pattern, subject, jumpOnFail = false).foreach { successJump =>
      transformationNotSupported(s"Cannot transform fail jump for $pattern")
    }

  @tailrec
  final def transformPatternWithCustomFail(pattern: ScPattern, subject: builder.Value, jumpOnFail: Boolean): Option[builder.UnlinkedJump] = pattern match {
    case ScParenthesisedPattern(pattern) => transformPatternWithCustomFail(pattern, subject, jumpOnFail)
    case pattern: ScReferencePattern =>
      builder.writeVariable(variable(pattern), subject)
      None
    case _ => transformationNotSupported(pattern)
  }

  final def buildThrowMatchError(): Unit = transformationNotSupported("Cannot create match error yet")
}
