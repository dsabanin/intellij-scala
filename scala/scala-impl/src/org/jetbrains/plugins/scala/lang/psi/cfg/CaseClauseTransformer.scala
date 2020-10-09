package org.jetbrains.plugins.scala.lang.psi.cfg

import org.jetbrains.plugins.scala.dfa.{DfBool, DfUnit}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScCaseClause, ScCaseClauses}

private trait CaseClauseTransformer { this: Transformer =>
  final def transformCaseClauses(caseClauses: ScCaseClauses, subject: builder.Value, rreq: ResultReq): rreq.Result[builder.Value] =
    transformCaseClauses(caseClauses.caseClauses, subject, rreq)

  final def transformCaseClauses(caseClauses: Seq[ScCaseClause],
                                 subject: builder.Value,
                                 rreq: ResultReq): rreq.Result[builder.Value] = {
    val endJumps = Seq.newBuilder[builder.UnlinkedJump]
    val resultVariable = rreq.ifNeeded(builder.newVariable("matchResult", new AnyRef))

    val scopeInfoAtCaseStart = builder.currentScopeInfo
    val lastCaseClauseIdx = caseClauses.size - 1
    val failJumps = caseClauses.iterator.zipWithIndex.foldLeft(Seq.empty[builder.UnlinkedJump]) {
      case (prevFailJumps, (ScCaseClause(Some(pattern), guard, expr), idx)) =>
        val isLast = idx == lastCaseClauseIdx

        val blockName = "case" + idx
        if (prevFailJumps.isEmpty) builder.allowDeadBlockHere(blockName, scopeInfoAtCaseStart)
        else builder.jumpHere(blockName, prevFailJumps)

        var failJumps = List.empty[builder.UnlinkedJump]

        // matcher
        failJumps ++= transformPatternWithCustomFail(pattern, subject, jumpOnFail = true)

        // guard
        failJumps ++= guard.map { guard =>
          val cond = transformExpressionOrDefault(guard.expr, DfBool.Top)
          builder.jumpToFutureIfNot(cond, afterBlockName = "caseBody")
        }

        // body
        transformAndWriteIfNeeded(expr, DfUnit.Concrete, resultVariable, rreq)

        val succeedsAlways = failJumps.isEmpty
        val canExitDirectly = isLast && succeedsAlways
        if (!canExitDirectly) {
          endJumps += builder.jumpToFuture()
        }

        failJumps
      case (nextLabel, _) =>
        nextLabel
    }

    if (failJumps.nonEmpty) {
      builder.jumpHere("caseClauseFail", failJumps)
      buildThrowMatchError()
    }

    builder.jumpHere("endCaseClause", endJumps.result())

    rreq.map(resultVariable)(builder.readVariable)
  }
}
