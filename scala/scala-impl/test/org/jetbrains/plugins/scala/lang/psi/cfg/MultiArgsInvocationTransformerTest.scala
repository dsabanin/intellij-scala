package org.jetbrains.plugins.scala.lang.psi.cfg

class MultiArgsInvocationTransformerTest extends TransformerTestBase {
  def test_multi_arglist(): Unit = {
    check(
      """
        |def test(aa: Int)(bb: Int) = ???
        |
        |test(3)(4)
      """.stripMargin,
      """
        |%0 <- DfInt(3)
        |%1 <- DfInt(4)
        |%2 <- call test(%0)(%1)
        |end
      """.stripMargin
    )
  }

  def test_returned_lambda(): Unit = {
    check(
      """
        |def test(aa: Int): Int => Unit = ???
        |
        |test(3)(4)
      """.stripMargin,
      """
        |%0 <- DfInt(3)
        |%1 <- call test(%0)
        |%2 <- DfInt(4)
        |%3 <- call %1.apply(%2)
        |end
      """.stripMargin
    )
  }

  def test_returned_lambda_from_multiple_arglists(): Unit = {
    check(
      """
        |def test(aa: Int)(bb: Int): Int => Unit = ???
        |
        |test(3)(4)(5)
      """.stripMargin,
      """
        |%0 <- DfInt(3)
        |%1 <- DfInt(4)
        |%2 <- call test(%0)(%1)
        |%3 <- DfInt(5)
        |%4 <- call %2.apply(%3)
        |end
      """.stripMargin
    )
  }

  def test_unresolved_function(): Unit = {
    check(
      """
        |test(3)(4)
      """.stripMargin,
      """
        |
        |%0 <- DfInt(3)
        |%1 <- DfInt(4)
        |%2 <- call <unknown>(%0)(%1)
        |end
      """.stripMargin
    )
  }

  def test_wrongly_called_returned_lambda(): Unit = {
    check(
      """
        |def test(aa: Int): Int => Unit = ???
        |
        |test(3)(4, 5)(6)
      """.stripMargin,
      """
        |%0 <- DfInt(3)
        |%1 <- call test(%0)
        |%2 <- DfInt(4)
        |%3 <- DfInt(5)
        |%4 <- call %1.apply(%2, %3)
        |%5 <- DfInt(6)
        |%6 <- call %4.<unknown>(%5)
        |end
      """.stripMargin
    )
  }

  def test_unresolved_lambda_call(): Unit = {
    check(
      """
        |def test(aa: Int): Int = 0
        |
        |test(3)(4, 5)(6)
      """.stripMargin,
      """
        |
        |%0 <- DfInt(3)
        |%1 <- call test(%0)
        |%2 <- DfInt(4)
        |%3 <- DfInt(5)
        |%4 <- DfInt(6)
        |%5 <- call %1.<unknown>(%2, %3)(%4)
        |end
      """.stripMargin
    )
  }

  def test_lambda_call_from_def(): Unit = {
    check(
      """
        |def test: Int => Int = ???
        |
        |test(3)
      """.stripMargin,
      """
        |%0 <- call test
        |%1 <- DfInt(3)
        |%2 <- call %0.apply(%1)
        |end
      """.stripMargin
    )
  }

  def test_default_args_ordering_in_multi_args(): Unit = {
    check(
      """
        |val a, b, arg1 = 0
        |def test(aa: Int = a)(bb: Int = b): Unit = ()
        |
        |test()(arg1)
        |""".stripMargin,
      """
        |%0 <- DfInt(0)
        |%1 <- DfInt(0)
        |%2 <- DfInt(0)
        |%3 <- call test(%0)(%2)
        |end
        |""".stripMargin
    )
  }

  def test_multiarglist_with_infix(): Unit = {
    check(
      """
        |val arg1, arg2 = 0
        |object Test {
        |  def ++ (i: Int)(j: Int) = ()
        |}
        |(Test ++ arg1)(arg2)
        |""".stripMargin,
      """
        |%0 <- DfInt(0)
        |%1 <- DfInt(0)
        |%2 <- call Test
        |%3 <- call %2.++(%0)(%1)
        |end
        |""".stripMargin
    )
  }

  def test_multiarglist_with_right_assoc_infix(): Unit = {
    check(
      """
        |object Test {
        |  def -: (i: Int)(j: Int) = ()
        |}
        |(0 -: Test)(1)
        |""".stripMargin,
      """
        |%0 <- DfInt(0)
        |%1 <- call Test
        |%2 <- DfInt(1)
        |%3 <- call %1.-:(%0)(%2)
        |end
        |""".stripMargin
    )
  }
}
