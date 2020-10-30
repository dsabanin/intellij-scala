package org.jetbrains.plugins.scala.lang.psi.cfg

import org.jetbrains.plugins.scala.lang.psi.api.expr.{MethodInvocation, ScExpression}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaCode.ScalaCodeContext

private trait TupleTransformation { this: Transformer =>
  final def transformTupleItems(items: Seq[ScExpression]): builder.Value = {
    buildTupleCreationForEntities(items.map(transformExpression))
  }

  final def buildTupleCreationForEntities(items: Seq[builder.Value]): builder.Value = {
    val invocation =
      code"Tuple${items.length}.apply(${items.map(_ => "???").mkString(", ")})"
      .asInstanceOf[MethodInvocation]
    val invocationInfo = invocationInfoFor(invocation)
    invocationInfo.transform(args = Seq(items))
  }
}
