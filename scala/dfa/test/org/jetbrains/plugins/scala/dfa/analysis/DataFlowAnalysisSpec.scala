package org.jetbrains.plugins.scala.dfa
package analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DataFlowAnalysisSpec extends AnyFunSuite with Matchers {
  private lazy val (arg, const, graph) = {
    val builder = cfg.Builder.newBuilder[AnyRef]()

    val argSource = new AnyRef
    val constSource = new AnyRef

    builder.withSourceInfo(argSource) {
      builder.addArgument("arg", new AnyRef)
    }

    builder.withSourceInfo(constSource) {
      builder.constant(DfInt(10))
    }

    val graph = builder.finish()
    (
      graph.valueForSource(argSource),
      graph.valueForSource(constSource),
      graph
    )
  }

  test("run without initializing") {
    val dfa = new DataFlowAnalysis(graph)
    dfa.run()

    dfa.hasFinished shouldBe true

    val results = dfa.finishedStates
    results.size shouldBe 1

    dfa.inspect(arg) shouldBe DfAny.Top
    dfa.inspect(const) shouldBe DfInt(10)
  }

  test("run with args") {
    val dfa = new DataFlowAnalysis(graph)
    dfa.init(Seq(DfInt(1)))
    dfa.run()

    dfa.inspect(arg) shouldBe DfInt(1)
  }

  test("wrong numbers of args") {
    val dfa = new DataFlowAnalysis(graph)

    an [AssertionError] should be thrownBy
      dfa.init(Seq(DfBool.True, DfBool.False))
  }
}
