package org.jetbrains.plugins.scala.lang.psi.cfg

import com.intellij.psi.PsiElement

private final class Transformer(val builder: Builder)
  extends PatternTransformer
  with ExpressionTransformer
  with StatementTransformation
  with CallTransformation
{
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