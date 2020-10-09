package org.jetbrains.plugins.scala.lang.psi.cfg

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.dfa._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.{ScalaElementVisitor, ScalaFile, ScalaPsiElement}

object PsiToCfgTransformation {
  final def transform(element: PsiElement): Option[PsiGraph] =
    try Some(transformUnsafe(element))
    catch { case _: UnsupportedTransformationException => None }

  private[cfg] def transformUnsafe(element: PsiElement): PsiGraph = {
    implicit val builder: Builder = cfg.Builder.newBuilder()
    val transformer = new Transformer(builder)

    element match {
      case file: ScalaFile if file.isScriptFile || file.isWorksheetFile =>
        file.acceptChildren(new ScalaElementVisitor {
          override def visitScalaElement(element: ScalaPsiElement): Unit =
            transformer.transformAny(element)
        })
      case fun: ScFunctionDefinition =>
        for (param <- fun.parameters) {
          builder.addArgument(param.name, param)
        }

        for (body <- fun.body) {
          transformer.transformExpression(body)
        }

      case element: ScalaPsiElement =>
        transformer.transformAny(element)

      case element =>
        transformationNotSupported(element)
    }

    builder.finish()
  }
}
