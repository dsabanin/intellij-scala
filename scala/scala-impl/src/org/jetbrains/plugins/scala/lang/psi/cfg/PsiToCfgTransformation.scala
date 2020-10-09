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

    element match {
      case file: ScalaFile if file.isScriptFile || file.isWorksheetFile =>
        val transformer = new Transformer(builder, thisVariable = None)
        file.acceptChildren(new ScalaElementVisitor {
          override def visitScalaElement(element: ScalaPsiElement): Unit =
            transformer.transformAny(element)
        })
      case fun: ScFunctionDefinition =>
        val transformer = new Transformer(builder, thisVariable = Some(builder.addArgument("this", new AnyRef)._1))
        for (param <- fun.parameters) {
          builder.addArgument(param.name, param)
        }

        for (body <- fun.body) {
          transformer.transformExpression(body)
        }

      case element: ScalaPsiElement =>
        val transformer = new Transformer(builder, thisVariable = None)
        transformer.transformAny(element)

      case element =>
        transformationNotSupported(element)
    }

    builder.finish()
  }
}
