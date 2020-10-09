package org.jetbrains.plugins.scala.lang.psi.cfg

import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlockStatement, ScExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScPatternDefinition, ScValueDeclaration, ScValueOrVariable, ScVariableDeclaration, ScVariableDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.ScImportStmt

import scala.annotation.tailrec

private trait StatementTransformation { this: Transformer =>
  final def transformStatements(stmts: Seq[ScBlockStatement], rreq: ResultReq): rreq.Result[builder.Value] = {
    val it = stmts.iterator
    if (!it.hasNext) {
      rreq.ifNeeded(buildUnit())
    } else {
      @tailrec
      def processNext(): rreq.Result[builder.Value] = {
        val stmt = it.next()
        val hasNext = it.hasNext
        if (!hasNext) transformStatement(stmt, rreq)
        else {
          transformStatement(stmt, ResultReq.NotNeeded)
          processNext()
        }
      }
      processNext()
    }
  }

  final def transformStatement(stmt: ScBlockStatement, rreq: ResultReq): rreq.Result[builder.Value] = attachSourceInfoIfSome(stmt) {
    val maybeResult = stmt match {
      case expression: ScExpression => transformExpression(expression, rreq)
      case function: ScFunction =>
        transformationNotSupported(function)
        None
      case stmt: ScImportStmt =>
        transformationNotSupported(stmt)
        None
      case variable: ScValueOrVariable =>
        transformValueOrVariable(variable)
        None
    }
    rreq.orIfNeeded(maybeResult, buildUnit())
  }

  final def transformValueOrVariable(variable: ScValueOrVariable): Unit = variable match {
    case patternDef: ScPatternDefinition => transformPatternList(patternDef.pList, patternDef.expr)
    case variableDef: ScVariableDefinition => transformPatternList(variableDef.pList, variableDef.expr)
    case _: ScValueDeclaration => // nothing to do
    case _: ScVariableDeclaration => // nothing to do
  }
}
