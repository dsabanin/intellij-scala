package org.jetbrains.plugins.scala
package lang
package psi

import com.intellij.psi.{PsiElement, PsiNamedElement}
import org.jetbrains.plugins.scala.extensions._

package object cfg {
  type PsiGraph = dfa.cfg.Graph[PsiElement]
  private[cfg] val Builder = dfa.cfg.Builder
  private[cfg] type Builder = dfa.cfg.Builder[PsiElement]

  private[cfg] def variable(named: PsiNamedElement): Builder.Variable =
    Builder.Variable(named)(named.name)


  private[cfg] class UnsupportedTransformationException(msg: String) extends Exception(msg)

  private[cfg] def transformationNotSupported(msg: String): Nothing =
    throw new UnsupportedTransformationException(msg)
  private[cfg] def transformationNotSupported(element: PsiElement): Nothing =
    throw new UnsupportedTransformationException(s"Couldn't transform $element")
}
