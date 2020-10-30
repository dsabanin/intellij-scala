package org.jetbrains.plugins.scala.lang.psi.cfg

import com.intellij.psi.{PsiMethod, PsiNamedElement}
import org.jetbrains.plugins.scala.dfa._
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.base.literals._
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScTypedExpression, _}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFun, ScParameterOwner}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveResult

private trait ExpressionTransformation { this: Transformer =>
  final def transformExpression(expr: ScExpression): builder.Value = transformExpression(expr, ResultReq.Needed).value
  final def transformExpressionOrDefault(expr: Option[ScExpression], default: DfAny): builder.Value =
    expr.fold(builder.constant(default))(transformExpression)
  final def transformExpressionOrDefault(expr: Option[ScExpression], default: DfAny, rreq: ResultReq): rreq.Result[builder.Value] =
    expr.fold(rreq.ifNeeded(builder.constant(default)))(transformExpression(_, rreq))


  final def transformExpression(expr: ScExpression, rreq: ResultReq): rreq.Result[builder.Value] = attachSourceInfoIfSome(expr) {
    Some(expr match {
        // **************** Literals **************** //
      case ScBooleanLiteral(bool) => builder.constant(DfBool(bool))
      case ScCharLiteral(char) => builder.constant(transformationNotSupported(expr))
      case ScIntegerLiteral(int) => builder.constant(DfInt(int))
      case ScLongLiteral(long) => builder.constant(transformationNotSupported(expr))
      case ScFloatLiteral(float) => builder.constant(transformationNotSupported(expr))
      case ScDoubleLiteral(double) => builder.constant(transformationNotSupported(expr))
      case _: ScNullLiteral => builder.constant(DfNull.Always)
      case _: ScUnitExpr => builder.constant(DfUnit.Concrete)
      case ScSymbolLiteral(_) => transformationNotSupported(expr)
      case ScStringLiteral(str) => builder.constant(new DfStringRef(str))

      // ***************** Reference ***************** //
      case reference: ScReferenceExpression => transformReference(reference)
      case _: ScThisReference => return buildThisValueOrNothing(rreq)

      // ***************** Call likes **************** //
      case ScTuple(exprs) => transformTupleItems(exprs)
      case call: ScMethodCall => transformMethodCall(call)
      case infixExpr: ScInfixExpr => transformInfixCall(infixExpr)
      case ScAssignment(leftInvocation: ScMethodCall, _) =>
        invocationInfoFor(leftInvocation)
          .copy(thisExpr = Some(leftInvocation.getEffectiveInvokedExpr))
          .transform()
      case ScAssignment(_, _) =>
        transformationNotSupported("assignment to something else then a method call")
        
      // Catch all remaining method invocations
      case invoc: MethodInvocation => invocationInfoFor(invoc).transform()

      // ******************* Block ******************* //
      case ScParenthesisedExpr(inner) => return transformExpression(inner, rreq)
      case typedExpr: ScTypedExpression => return transformExpression(typedExpr.expr, rreq)
      case block: ScBlock => return transformStatements(block.statements, rreq)


      // *************** Control stuff *************** //
      case ScIf(condExpr, thenExpr, elseExpr) =>
        // if
        val condValue = transformExpressionOrDefault(condExpr, DfBool.Top)
        val elseOrEndJump = builder.jumpToFutureIfNot(condValue, afterBlockName = "then")
        val resultVariable = rreq.ifNeeded(builder.newVariable("ifResult", new AnyRef))

        // then
        transformAndWriteIfNeeded(thenExpr, DfNothing, resultVariable, rreq)

        // else
        val endJump = if (rreq.needed || elseExpr.isDefined) {
          val endJump = builder.jumpToFuture()
          builder.jumpHere("else", elseOrEndJump)
          transformAndWriteIfNeeded(elseExpr, DfUnit.Concrete, resultVariable, rreq)

          endJump
        } else elseOrEndJump

        // end
        builder.jumpHere("endIf", endJump)
        return rreq.map(resultVariable)(builder.readVariable)

      case scMatch: ScMatch =>
        val subject = transformExpressionOrDefault(scMatch.expression, DfAny.Top)
        return transformCaseClauses(scMatch.caseClauses, subject, rreq)

      case ret: ScReturn =>
        val scopeInfo = builder.currentScopeInfo
        ret.expr match {
          case Some(returnValue) => builder.ret(transformExpression(returnValue))
          case None => builder.ret()
        }
        builder.allowDeadBlockHere("deadCodeAfterReturn", scopeInfo)
        return rreq.ifNeeded(buildUnit())

      case e => transformationNotSupported(e)
    })
  }

  final def transformReference(reference: ScReferenceExpression): builder.Value = attachSourceInfo(reference) {
    reference.bind() match {
      case Some(ResolvesToObject(obj)) =>
        InvocationInfo(None, Some(obj), Seq.empty)
          .transform()
        
      case Some(result) if reference.refName != result.name && !(result.name == "apply" || result.name == "update") =>
        builder.readVariable(variable(result.parentElement.get))

      case Some(ResolvesToFunction(func)) =>
        InvocationInfo(reference.qualifier, Some(func), Seq.empty)
          .transform()

      case Some(result) =>
        builder.readVariable(variable(result.element))

      case None =>
        // we have to be able to handle the error here
        transformationNotSupported(reference)
    }
  }

  final def buildAny(): builder.Value = builder.constant(DfAny.Top)
  final def buildNothing(): builder.Value = builder.constant(DfNothing)
  final def buildUnit(): builder.Value = builder.constant(DfUnit.Concrete)

  final def buildThisValueOrNothing(rreq: ResultReq): rreq.Result[builder.Value] = thisVariable match {
    case Some(thisVariable) => Some(builder.readVariable(thisVariable))
    case None => rreq.ifNeeded(buildAny())
  }

  final object ResolvesToFunction {
    def unapply(result: ScalaResolveResult): Option[PsiNamedElement] = result match {
      case ScalaResolveResult(scalaFun: ScParameterOwner, _) => Some(scalaFun)
      case ScalaResolveResult(syntheticFun: ScFun, _) => Some(syntheticFun)
      case ScalaResolveResult(javaFun: PsiMethod, _) => Some(javaFun)
      case _ => None
    }
  }

  final object ResolvesToObject {
    def unapply(result: ScalaResolveResult): Option[ScObject] =
      result.getActualElement.asOptionOf[ScObject]
  }

  final def transformAndWriteIfNeeded(expr: Option[ScExpression], default: DfAny, resultVariable: Option[Builder.Variable], rreq: ResultReq): Unit = {
    val maybeResult = expr.flatMap(transformExpression(_, rreq))
    for (resultVariable <- resultVariable) {
      val result = maybeResult.getOrElse(builder.constant(default))
      builder.writeVariable(resultVariable, result)
    }
  }
}
