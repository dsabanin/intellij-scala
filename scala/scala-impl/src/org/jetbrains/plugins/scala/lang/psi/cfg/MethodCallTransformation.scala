package org.jetbrains.plugins.scala.lang.psi.cfg

import com.intellij.psi.PsiMethod
import org.jetbrains.plugins.scala.lang.psi.api.expr.{MethodInvocation, ScAssignment, ScExpression, ScInfixExpr, ScMethodCall}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFun, ScParameterOwner}
import org.jetbrains.plugins.scala.lang.psi.types.api
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.Parameter
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveResult

import scala.annotation.tailrec

private trait MethodCallTransformation { this: Transformer =>

  final def transformMethodCall(methodCall: ScMethodCall): builder.Value = {
    val innerChain = innerMethodCallChain(methodCall)
    val relevantCalls = splitChain(innerChain)

    @tailrec
    def transformOuters(invocs: List[CallInfo], prevResult: builder.Value): builder.Value = invocs match {
      case Nil => prevResult
      case next :: rest =>
        val result = toInvocationInfo(next).transform(thisRef = Some(prevResult))
        transformOuters(rest, result)
    }

    relevantCalls match {
      case (mostInnerCallInfo@(mostInnerTarget, _, _)) :: rest =>
        val invocationInfo = toInvocationInfo(mostInnerCallInfo)
        val mostInnerResult = mostInnerTarget match {
          case infix: ScInfixExpr if infix.isRightAssoc => invocationInfo.transformRightAssoc()
          case _ => invocationInfo.transform()
        } 
        transformOuters(rest, mostInnerResult)
      case Nil => transformationNotSupported("method call with no relevant call should not be possible")
    }
  }

  private type CallInfo = (MethodInvocation, Option[ScalaResolveResult], Seq[ArgParamClause])
  private def toInvocationInfo(callInfo: CallInfo): InvocationInfo = {
    val (call, resolveResult, matchedParameters) = callInfo
    InvocationInfo(call.thisExpr, resolveResult.map(_.element), matchedParameters)
  }
  private def splitChain(chain: List[MethodInvocation]): List[CallInfo] = {
    def gatherCalls(restCalls: List[(MethodInvocation, Option[ScalaResolveResult])]): List[CallInfo] = {
      if (restCalls.isEmpty)
        return Nil
      val (call, result) :: rest = restCalls
      val target = result.map(_.mostInnerResolveResult)
      val tuplingUsed = target.exists(_.tuplingUsed)
      val (restArgs, followingCalls) = target match {
        case Some(ScalaResolveResult(scalaFun: ScParameterOwner, _)) => rest.splitAt(scalaFun.allClauses.length - 1)
        case Some(ScalaResolveResult(syntheticFun: ScFun, _)) => rest.splitAt(syntheticFun.paramClauses.length - 1)
        case Some(ScalaResolveResult(javaFun: PsiMethod, _)) => (Nil, rest)
        case None => rest.span(_._2.isEmpty)
      }

      val allMatchedArgs = call.matchedParameters +: restArgs.map(_._1.matchedParameters)
      val allArgExprs = call.argumentExpressions +: restArgs.map(_._1.argumentExpressions)

      // there might be more parameter then the function wants. In this case still evaluate the parameters.
      val fixedArgs =
        allArgExprs.zip(allMatchedArgs)
          .map { case (exprs, argParams) => fixArguments(exprs, argParams, tuplingUsed) }

      (call, target, fixedArgs) :: gatherCalls(followingCalls)
    }

    gatherCalls(chain.map(call => call -> call.target))
  }

  private def fixArguments(args: Seq[ScExpression], matched: Seq[(ScExpression, Parameter)], tuplingUsed: Boolean): ArgParamClause = {
    def notAlreadyMatched(arg: ScExpression): Boolean =
      !matched.exists(_._1 == arg)

    val notMatchedArgs = args.filter {
      case ScAssignment(_, Some(actualArg)) => notAlreadyMatched(actualArg)
      case arg => notAlreadyMatched(arg)
    }
    ArgParamClause(matched ++ makeFakeParameters(notMatchedArgs, matched.length), isTupled = tuplingUsed)
  }

  // arguments to an unresolved call should still be evaluated
  private def makeFakeParameters(args: Seq[ScExpression], initialIndex: Int): Seq[(ScExpression, Parameter)] = {
    for ((arg, idx) <- args.zipWithIndex) yield {
      arg -> Parameter(api.Any, isRepeated = false, index = idx + initialIndex)
    }
  }

  private def innerMethodCallChain(methodCall: ScMethodCall): List[MethodInvocation] = {
    @tailrec
    def inner(cur: ScExpression, found: List[MethodInvocation]): List[MethodInvocation] = {
      cur match {
        case call: ScMethodCall => inner(call.getEffectiveInvokedExpr, call :: found)
        case infix: ScInfixExpr => infix :: found
        case _ => found
      }
    }

    inner(methodCall.getEffectiveInvokedExpr, methodCall :: Nil)
  }
}
