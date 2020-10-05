package org.jetbrains.plugins.scala.lang.psi.cfg

import org.jetbrains.plugins.scala.dfa._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition

import scala.util.control.Exception.catching

object PsiToCfgTransformation {
  private val unsupportedTransformationCatch = catching(classOf[UnsupportedTransformationException])

  final def transform(fun: ScFunctionDefinition): Option[PsiGraph] = unsupportedTransformationCatch.opt {
    implicit val builder: Builder = cfg.Builder.newBuilder()

    for (param <- fun.parameters) {
      builder.addArgument(param.name, param)
    }

    val transformer = new Transformer(builder)
    for (body <- fun.body) {
      transformer.transformExpression(body)
    }

    builder.finish()
  }
}
