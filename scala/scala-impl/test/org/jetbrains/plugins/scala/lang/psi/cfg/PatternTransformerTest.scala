package org.jetbrains.plugins.scala.lang.psi.cfg

class PatternTransformerTest extends TransformerTestBase {
  def test_val(): Unit = {
    check(
      """
        |val a = 5
      """.stripMargin,
      """
        |%0 <- DfInt(5)
        |end
      """.stripMargin
    )
  }

  def test_multiple_val(): Unit = {
    check(
      """
        |val a, b = 1
      """.stripMargin,
      """
        |%0 <- DfInt(1)
        |%1 <- DfInt(1)
        |end
      """.stripMargin
    )
  }

  def test_var(): Unit = {
    check(
      """
        |var a = 5
      """.stripMargin,
      """
        |%0 <- DfInt(5)
        |end
      """.stripMargin
    )
  }

  def test_multiple_var(): Unit = {
    check(
      """
        |var a, b = 1
      """.stripMargin,
      """
        |%0 <- DfInt(1)
        |%1 <- DfInt(1)
        |end
      """.stripMargin
    )
  }

  def test_wildcard(): Unit = {
    check(
      """
        |val _ = 3
        |""".stripMargin,
      """
        |%0 <- DfInt(3)
        |end
        |""".stripMargin
    )

    check(
      """
        |val _, _ = 3
        |""".stripMargin,
      """
        |%0 <- DfInt(3)
        |%1 <- DfInt(3)
        |end
        |""".stripMargin
    )
  }
}