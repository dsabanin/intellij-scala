package org.jetbrains.plugins.scala.dfa
package cfg

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BuilderSpec extends AnyFunSuite with Matchers with BuilderMatchers {
  def newBuilder: Builder[Nothing] = Builder.newBuilder()

  test("single const") {
    val builder = newBuilder
    builder.constant(DfBool.True)

    builder.finish() should disassembleTo(
      """
        |  %0 <- DfTrue
        |  end
        |""".stripMargin
    )
  }

  test("jump to end") {
    val builder = newBuilder
    val jump = builder.jumpToFuture()
    builder.jumpHere("label", jump)

    builder.finish() should disassembleTo(
      """
        |  jump .label[1]
        |.label[1]:
        |  end
        |""".stripMargin
    )
  }

  test("jump to end conditionally") {
    val builder = newBuilder
    val cond = builder.constant(DfBool.Top)
    val jump = builder.jumpToFutureIfNot(cond, "nope")
    builder.jumpHere("end", jump)

    builder.finish() should disassembleTo(
      """
        |  %0 <- DfBool.Top
        |  if not %0 jump .end[2]
        |.end[2]:
        |  end
        |""".stripMargin
    )
  }

  test("phi after merge") {
    val builder = newBuilder
    val resultVar = builder.freshVariable()

    val cond = builder.constant(DfBool.Top)
    val jump = builder.jumpToFutureIfNot(cond, "then")

    // then
    val inThen = builder.constant(DfInt(3))
    builder.writeVariable(resultVar, inThen)
    val toEnd = builder.jumpToFuture()

    // else
    builder.jumpHere("else", jump)
    val inElse = builder.constant(DfInt(10))
    builder.writeVariable(resultVar, inElse)

    // end
    builder.jumpHere("after", toEnd)

    builder.finish() should disassembleTo(
      """
        |  %0 <- DfBool.Top
        |  if not %0 jump .else[2]
        |  %1 <- DfInt(3)
        |  jump .after[3]
        |.else[2]:
        |  %2 <- DfInt(10)
        |.after[3]:
        |  phi %3 <- %1 | %2
        |  end
        |""".stripMargin
    )
  }

  test("arguments") {
    val builder = newBuilder

    builder.addArgument("arg1", new AnyRef)
    builder.addArgument("arg2", new AnyRef)

    builder.finish() should disassembleTo(
      """
        |  %0 <- arg1 [argument 0]
        |  %1 <- arg2 [argument 1]
        |  end
        |""".stripMargin
    )
  }

  test("arguments cannot be added after other nodes") {
    val builder = newBuilder

    builder.constant(DfInt.Concrete(3))

    an [AssertionError] should be thrownBy
      builder.addArgument("blub", new AnyRef)
  }

  test("already used labels are rejected") {
    val builder = newBuilder

    val jump = builder.jumpToFuture()

    builder.jumpHere("test", jump)

    an [AssertionError] should be thrownBy
      builder.jumpHere("fail", jump)
  }
}
