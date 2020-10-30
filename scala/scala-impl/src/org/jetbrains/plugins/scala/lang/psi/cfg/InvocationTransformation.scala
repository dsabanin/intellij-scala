package org.jetbrains.plugins.scala.lang.psi.cfg

import com.intellij.psi.{PsiElement, PsiNameIdentifierOwner}
import org.jetbrains.plugins.scala.dfa.cfg.CallInfo
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.Parameter

import scala.annotation.nowarn

private trait InvocationTransformation { this: Transformer =>
  /*
    TODO: We have the following cases when we encounter a method invocation
    - [x] calls to functions
    - [x] calls to methods (=> need thisref)
    - [x] calls to function objects
    - [x] calls to apply
    - [x] calls to update
    - [~] calls with assignment (i.e. +=, -=, ::=)
    - [ ] property updates (a.prop = 33 -> a.prop_=(33))
    - [x] calls that have changed precedence (::, everything with : as last char)
    - [ ] imported member methods
    - [ ] with or without implicits
    - [x] with or without generics
    - [x] multiple parameter clauses
    - [x] auto tupeling
    - [x] default parameter
    - [x] named arguments (potentially reordered)
    - [ ] varargs
    - [ ] dynamics
   */

  @nowarn("msg=The outer reference in this type test cannot be checked at run time")
  final case class ArgParamClause(argParams: Seq[(ScExpression, Parameter)], isTupled: Boolean) {
    def sortedByExprPosition: ArgParamClause =
      if (isTupled) this
      else copy(argParams = argParams.sortBy(ArgumentSorting.exprPosition))

    def build(): Seq[(Int, builder.Value)] = {
      if (isTupled) Seq(0 -> transformTupleItems(argParams.map(_._1)))
      else argParams.map(buildArgParams)
    }
  }

  private def buildArgParams(argParam: (ScExpression, Parameter)): (Int, builder.Value) = argParam match {
    case (blockWithCaseClause@ScBlockExpr.withCaseClauses(_), param) =>
      //param.index -> CaseClausesTools.createLambdaFromCaseClausesBlock(blockWithCaseClause)
      transformationNotSupported("Call with block case clauses")
    case (expr, param) =>
      param.index -> transformExpression(expr)
  }

  @nowarn("msg=The outer reference in this type test cannot be checked at run time")
  final case class InvocationInfo(thisExpr: Option[ScExpression],
                                  funcRef: Option[PsiElement],
                                  argParams: Seq[ArgParamClause]) {
    def transform(thisRef: => Option[builder.Value] = transformThisRef(),
                  args: => Seq[Seq[builder.Value]] = transformArgs(),
                  callInfo: CallInfo = callInfo): builder.Value =
      builder.call(callInfo, thisRef, args)

    def transformRightAssoc(callInfo: CallInfo = callInfo): builder.Value = {
      assert(this.argParams.nonEmpty)
      assert(!this.argParams.head.isTupled)
      // For 'a :: b'
      // 1. evaluate a
      // 2. evaluate b
      // 3. evaluate default and implicit parameters
      val firstArgClause +: restClauses = this.argParams.map(_.sortedByExprPosition)
      val (leftExpr, _) +: defaultOrImplicitParams = firstArgClause.argParams

      val actualArgRef = transformExpression(leftExpr)
      val thisRef = transformThisRef()
      val defaultOrImplicitArgRefs = defaultOrImplicitParams.map(buildArgParams).map(_._2)
      val firstArgClauseRefs = actualArgRef +: defaultOrImplicitArgRefs
      val restArgClausesRefs = restClauses.map(_.build().map(_._2))

      builder.call(callInfo, thisRef, firstArgClauseRefs +: restArgClausesRefs)
    }

    def callInfo: CallInfo = {
      val name = funcRef match {
        case Some(named: PsiNameIdentifierOwner) => named.name
        case _ => "<unknown>"
      }
      CallInfo(name, isStatic = false)
    }

    def transformThisRef(): Option[builder.Value] =
      thisExpr.map(transformExpression)

    def transformArgs(): Seq[Seq[builder.Value]] = {
      val paramRegs = argParams.map(_.sortedByExprPosition.build())
      paramRegs.map(_.sortBy(ArgumentSorting.paramPosition).map(_._2))
    }
  }
  
  private object ArgumentSorting {
    def exprPosition(t: (ScExpression, Parameter)): (Int, Int) = {
      val (expr, param) = t
      // actually supplied arguments have to be evaluated before default parameters
      val notDefault = expr.parent.exists(!_.is[ScParameter])
      if (notDefault) 0 -> expr.getTextOffset
      else 1 -> param.index
    }
    def paramPosition(t: (Int, _)): Int = t._1
  }

  final def invocationInfoFor(invoc: MethodInvocation): InvocationInfo = {
    val rr = invoc.target
    val isTupled = rr.exists(_.tuplingUsed)
    InvocationInfo(
      invoc.thisExpr,
      rr.map(_.element),
      Seq(ArgParamClause(invoc.matchedParameters, isTupled))
    )
  }
}
