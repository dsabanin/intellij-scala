package org.jetbrains.plugins.scala.codeInspection.dfa

import com.intellij.codeInspection._
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.ScalaInspectionBundle
import org.jetbrains.plugins.scala.dfa.analysis.DfaResult
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.ScalaElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition

class ReturnsNullInspection extends LocalInspectionTool {
  override final def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): ScalaElementVisitor = new ScalaElementVisitor {
    override def visitFunctionDefinition(fun: ScFunctionDefinition): Unit =
      for {
        result <- fun.dataFlowResult
        expr <- fun.returnUsages
      } checkReturn(expr, result)

    private def checkReturn(expr: ScExpression, result: DfaResult[PsiElement]): Unit = {
      if (result.valueOf(expr).canLikelyBeNull) {
        holder.registerProblem(expr, ScalaInspectionBundle.message("this.might.return.null.consider.adding.nullable"))
      }
    }
  }
}
