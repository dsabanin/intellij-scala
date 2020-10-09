package org.jetbrains.plugins.scala.lang.psi.cfg

import org.jetbrains.plugins.scala.AssertionMatchers
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter

abstract class TransformerTestBase extends ScalaLightCodeInsightFixtureTestAdapter with AssertionMatchers {
  def check(code: String, result: String): Unit = {
    val actualFile = configureFromFileText(code)
    val graph = PsiToCfgTransformation.transformUnsafe(actualFile)

    graph.asmText().trim shouldBe result.trim
  }
}
