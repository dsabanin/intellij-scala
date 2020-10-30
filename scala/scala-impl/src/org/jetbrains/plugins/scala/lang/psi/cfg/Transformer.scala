package org.jetbrains.plugins.scala.lang.psi.cfg

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScBlockStatement
import org.jetbrains.plugins.scala.project.ProjectContext

private final class Transformer(val builder: Builder, val thisVariable: Option[Builder.Variable], val project: Project)
  extends PatternTransformation
  with ExpressionTransformation
  with StatementTransformation
  with InvocationTransformation
  with MethodCallTransformation
  with InfixCallTransformation
  with CaseClauseTransformation
  with TupleTransformation
{
  private[cfg] implicit val projectContext: ProjectContext = project

  def transformAny(element: ScalaPsiElement): Unit = element match {
    case stmt: ScBlockStatement => transformStatement(stmt, ResultReq.NotNeeded)
    case _ => // do nothing
  }

  def attachSourceInfo[T <: builder.Value](psiElement: PsiElement)(body: => T): T = {
    val value = body
    builder.addSourceInfo(value, psiElement)
    value
  }

  def attachSourceInfoIfSome[O <: Option[_ <: builder.Value]](psiElement: PsiElement)(body: => O): O = {
    val value = body
    value.foreach(builder.addSourceInfo(_, psiElement))
    value
  }
}