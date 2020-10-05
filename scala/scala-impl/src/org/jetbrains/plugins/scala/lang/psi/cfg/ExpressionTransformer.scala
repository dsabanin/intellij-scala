package org.jetbrains.plugins.scala.lang.psi.cfg

import com.intellij.psi.{PsiMethod, PsiNamedElement}
import org.jetbrains.plugins.scala.dfa._
import org.jetbrains.plugins.scala.lang.psi.api.base.literals._
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFun, ScParameterOwner}
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveResult

private trait ExpressionTransformer { this: Transformer =>
  final def transformExpression(expr: ScExpression): builder.Value = builder.withSourceInfo(expr) {
    expr match {
        // **************** Literals **************** //
      case ScBooleanLiteral(bool) => builder.constant(DfBool(bool))
      case ScCharLiteral(char) => builder.constant(transformationNotSupported)
      case ScIntegerLiteral(int) => builder.constant(DfInt(int))
      case ScLongLiteral(long) => builder.constant(transformationNotSupported)
      case ScFloatLiteral(float) => builder.constant(transformationNotSupported)
      case ScDoubleLiteral(double) => builder.constant(transformationNotSupported)
      case _: ScNullLiteral => builder.constant(DfNull.Concrete)
      case ScSymbolLiteral(_) => transformationNotSupported
      case ScStringLiteral(_) => transformationNotSupported

      // ***************** Reference **************** //
      case reference: ScReferenceExpression => transformReference(reference)

      // ******************* Block ****************** //
      case ScParenthesisedExpr(inner) => transformExpression(inner)
      case block: ScBlockExpr => transformBlock(block.statements)


      // *************** Control stuff *************** //
      case ScIf(condExpr, thenExpr, elseExpr) =>
        // if
        val condValue = condExpr.fold(builder.constant(DfBool.Top))(transformExpression)
        val elseJump = builder.jumpToFutureIfNot(condValue, "then")

        // then
        val resultVariable = builder.newVariable("ifResult", new AnyRef)
        val thenValue = thenExpr.fold(builder.constant(DfNothing))(transformExpression)
        builder.writeVariable(resultVariable, thenValue)
        val endJump = builder.jumpToFuture()

        // else
        builder.jumpHere("else", elseJump)
        val elseValue = elseExpr.fold(builder.constant(DfUnit.Concrete))(transformExpression)
        builder.writeVariable(resultVariable, elseValue)

        // end
        builder.jumpHere("endIf", endJump)
        builder.readVariable(resultVariable)

      case _ => transformationNotSupported
    }
  }

  final def transformReference(reference: ScReferenceExpression): builder.Value = builder.withSourceInfo(reference) {
    reference.bind() match {
      case Some(result) if reference.refName != result.name && (result.name == "apply" || result.name == "update") =>
        builder.readVariable(variable(result.parentElement.get))

      case Some(ResolvesToFunction(func)) =>
        //InvocationInfo(this.qualifier, Some(func), Seq.empty)
        //  .build(rreq)
        transformationNotSupported

      case Some(result) =>
        builder.readVariable(variable(result.element))

      case None =>
        // we have to be able to handle the error here
        transformationNotSupported
    }
  }

  object ResolvesToFunction {
    def unapply(result: ScalaResolveResult): Option[PsiNamedElement] = result match {
      case ScalaResolveResult(scalaFun: ScParameterOwner, _) => Some(scalaFun)
      case ScalaResolveResult(syntheticFun: ScFun, _) => Some(syntheticFun)
      case ScalaResolveResult(javaFun: PsiMethod, _) => Some(javaFun)
      case _ => None
    }
  }
}
