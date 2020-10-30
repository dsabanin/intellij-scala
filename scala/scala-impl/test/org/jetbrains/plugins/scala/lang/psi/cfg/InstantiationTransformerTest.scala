package org.jetbrains.plugins.scala.lang.psi.cfg

class InstantiationTransformerTest extends TransformerTestBase {
  def test_simple_creation(): Unit = check(
    """
      |class Test
      |
      |new Test
    """.stripMargin,
    """
      |%0 <- new Test
      |%1 <- call %0.constructor
      |end
    """.stripMargin
  )

  def test_creation_with_empty_param_clause(): Unit = check(
    """
      |class Test()
      |
      |new Test()
    """.stripMargin,
    """
      |%0 <- new Test
      |%1 <- call %0.constructor()
      |end
    """.stripMargin
  )

  def test_creation_with_arguments(): Unit = check(
    """
      |class Test(i: Int, j: Boolean)
      |
      |new Test(3, true)
    """.stripMargin,
    """
      |%0 <- new Test
      |%1 <- DfInt(3)
      |%2 <- DfTrue
      |%3 <- call %0.constructor(%1, %2)
      |end
    """.stripMargin
  )

  def test_creation_with_arguments_list(): Unit = check(
    """
      |class Test(i: Int, j: Boolean)(n: Int, m: Int)
      |
      |new Test(3, true)(4, 5)
    """.stripMargin,
    """
      |%0 <- new Test
      |%1 <- DfInt(3)
      |%2 <- DfTrue
      |%3 <- DfInt(4)
      |%4 <- DfInt(5)
      |%5 <- call %0.constructor(%1, %2)(%3, %4)
      |end
    """.stripMargin
  )

  def test_creation_of_new_template(): Unit = check(
    """
      |class Test(i: Int)
      |
      |new Test(1) { def fun(): Int = 0 }
    """.stripMargin,
    """
      |%0 <- new Test { def fun(): Int }
      |%1 <- DfInt(1)
      |%2 <- call %0.constructor(%1)
      |end
    """.stripMargin
  )
}
