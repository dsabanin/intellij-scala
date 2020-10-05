package org.jetbrains.plugins.scala.lang.psi.cfg

private final class Transformer(val builder: Builder)
  extends PatternTransformer
  with ExpressionTransformer
  with StatementTransformation
  with CallTransformation