package org.jetbrains.plugins.scala.lang.psi.cfg

class AssignmentTransformerTest extends TransformerTestBase {
    def test_assign_on_variable(): Unit = {
      check(
        """
          |var test = 32
          |test = 44
        """.stripMargin,
        """
          |%0 <- DfInt(32)
          |%1 <- DfInt(44)
          |end
        """.stripMargin
      )
    }

  /*
  todo: implement this
  def test_assign_on_member_variable(): Unit = {
    check(
      """
        |object Test {
        |  var test = 0
        |}
        |Test.test = 99
      """.stripMargin,
      """
        |write [Test] test <- 99
        |end
      """.stripMargin
    )
  }*/

  /*
  def test_assign_on_property(): Unit = {
    check(
      """
        |object Test {
        |  def prop = 3
        |  def prop_=(i: Int) = i
        |}
        |Test.prop = 42
      """.stripMargin,
      """
        |%0 <- callTest
        |%1 <- DfInt(42)
        |call %0.prop_=(%1)
        |end
      """.stripMargin
    )
  }*/

  def test_assign_on_indexer(): Unit = {
    check(
      """
        |object Test {
        |  def apply(idx: Int) = 3
        |  def update(idx: Int, value: Int) = value
        |}
        |val idx = 88
        |Test(idx) = 99
      """.stripMargin,
      """
        |%0 <- DfInt(88)
        |%1 <- call Test
        |%2 <- DfInt(99)
        |%3 <- call %1.update(%0, %2)
        |end
      """.stripMargin
    )
  }
}
