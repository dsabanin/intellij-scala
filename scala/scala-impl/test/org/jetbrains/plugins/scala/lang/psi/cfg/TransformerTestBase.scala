package org.jetbrains.plugins.scala.lang.psi.cfg

import org.jetbrains.plugins.scala.AssertionMatchers
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.jetbrains.plugins.scala.extensions._

abstract class TransformerTestBase extends ScalaLightCodeInsightFixtureTestAdapter with AssertionMatchers {
  def check(code: String, result: String): Unit = {
    val actualFile = configureFromFileText(code)
    val graph = actualFile.controlFlowGraph.ensuring(_.isDefined).get

    graph.asmText().trim shouldBe result.trim
  }
}
