package org.jetbrains.plugins.scala
package lang
package psi

import com.intellij.psi.{PsiElement, PsiNamedElement}
import org.jetbrains.plugins.scala.dfa.cfg.Builder
import org.jetbrains.plugins.scala.extensions._

package object cfg {
  type PsiGraph = dfa.cfg.Graph[PsiElement]
  private[cfg] type Builder = dfa.cfg.Builder[PsiElement]

  private[cfg] def variable(named: PsiNamedElement): Builder.Variable =
    Builder.Variable(named)(named.name)


  private[cfg] class UnsupportedTransformationException extends Exception

  private[cfg] def transformationNotSupported: Nothing = throw new UnsupportedTransformationException
}
