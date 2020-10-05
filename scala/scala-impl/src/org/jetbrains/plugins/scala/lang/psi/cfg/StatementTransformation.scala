package org.jetbrains.plugins.scala.lang.psi.cfg

import org.jetbrains.plugins.scala.dfa.DfUnit
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlockStatement, ScExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScPatternDefinition, ScValueDeclaration, ScValueOrVariable, ScVariableDeclaration, ScVariableDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.ScImportStmt

private trait StatementTransformation { this: Transformer =>
  final def transformBlock(stmts: Seq[ScBlockStatement]): builder.Value = {
    stmts.foldLeft(Option.empty[builder.Value]) {
      (_, stmt) => transformStatement(stmt)
    }.getOrElse(builder.constant(DfUnit.Concrete))
  }

  final def transformStatement(stmt: ScBlockStatement): Option[builder.Value] = builder.withSourceInfo(stmt) {
    stmt match {
      case expression: ScExpression => Some(transformExpression(expression))
      case function: ScFunction =>
        transformationNotSupported
        None
      case stmt: ScImportStmt =>
        transformationNotSupported
        None
      case variable: ScValueOrVariable =>
        transformValueOrVariable(variable)
        None
    }
  }

  final def transformValueOrVariable(variable: ScValueOrVariable): Unit = variable match {
    case patternDef: ScPatternDefinition => transformPatternList(patternDef.pList, patternDef.expr)
    case variableDef: ScVariableDefinition => transformPatternList(variableDef.pList, variableDef.expr)
    case _: ScValueDeclaration => // nothing to do
    case _: ScVariableDeclaration => // nothing to do
  }
}
