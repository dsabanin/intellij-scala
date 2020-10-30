package org.jetbrains.plugins.scala.lang.psi.cfg

class InvocationTransformerTest extends TransformerTestBase {
  def test_simple_call(): Unit = {
    check(
      """
        |def test(): Unit = ()
        |test()
      """.stripMargin,
      """
        |%0 <- call test()
        |end
      """.stripMargin
    )
  }

  def test_simple_call_withResult(): Unit = {
    check(
      """
        |def test(): Unit = ()
        |val a = test()
      """.stripMargin,
      """
        |%0 <- call test()
        |end
      """.stripMargin
    )
  }

  def test_call_to_object_method(): Unit = {
    check(
      """
        |object Test {
        |  def test(): Unit = ()
        |}
        |Test.test()
      """.stripMargin,
      """
        |%0 <- call Test
        |%1 <- call %0.test()
        |end
      """.stripMargin
    )
  }

  def test_call_to_renamed_object_method(): Unit = {
    check(
      """
        |object Test {
        |  def test(): Unit = ()
        |}
        |val a = Test
        |a.test()
      """.stripMargin,
      """
        |%0 <- call Test
        |%1 <- call %0.test()
        |end
      """.stripMargin
    )
  }

  def test_simple_args(): Unit = {
    check(
      """
        |def test(a: Int, b: String): Unit = ()
        |val a = 5
        |test(a, "test")
      """.stripMargin,
      """
        |%0 <- DfInt(5)
        |%1 <- DfString("test")
        |%2 <- call test(%0, %1)
        |end
      """.stripMargin
    )
  }

  def test_block_args(): Unit = {
    check(
      """
        |def test(a: Int): Unit = ()
        |
        |test {
        |  val a = 9
        |  a
        |}
      """.stripMargin,
      """
        |%0 <- DfInt(9)
        |%1 <- call test(%0)
        |end
      """.stripMargin
    )
  }

  def test_named_args(): Unit = {
    check(
      """
        |def test(aa: Int, bb: Int): Unit = ()
        |
        |val a = 1
        |val b = 2
        |
        |test(a, bb = b)
      """.stripMargin,
      """
        |%0 <- DfInt(1)
        |%1 <- DfInt(2)
        |%2 <- call test(%0, %1)
        |end
      """.stripMargin
    )
  }

  def test_reordered_named_args(): Unit = {
    check(
      """
        |def test(aa: Int, bb: Int): Unit = ()
        |
        |test(bb = 1, aa = 2)
      """.stripMargin,
      """
        |%0 <- DfInt(1)
        |%1 <- DfInt(2)
        |%2 <- call test(%1, %0)
        |end
      """.stripMargin
    )
  }

  def test_non_context_default_args(): Unit = {
    check(
      """
        |def test(aa: Int = 5): Unit = ()
        |test()
      """.stripMargin,
      """
        |%0 <- DfInt(5)
        |%1 <- call test(%0)
        |end
      """.stripMargin
    )
  }

  /*
  def test_context_default_args(): Unit = {
    check(
      """
        |object Test {
        |  val default = 5
        |  def test(aa: Int = default): Unit = ()
        |}
        |Test.test()
      """.stripMargin,
      """
        |%0 <- Test$
        |%1 <- default
        |call [%0](%1) Test$.test
        |end
      """.stripMargin
    )
  }
   */
  def test_context_default_args_ordering(): Unit = {
    check(
      """
        |val a, b, c, arg1, arg2 = 0
        |def test(aa: Int = a, bb: Int = b, cc: Int = c): Unit = ()
        |
        |// actually supplied arguments have to be evaluated before default parameters
        |test(arg1, cc = arg2)
      """.stripMargin,
      """
        |%0 <- DfInt(0)
        |%1 <- DfInt(0)
        |%2 <- DfInt(0)
        |%3 <- DfInt(0)
        |%4 <- DfInt(0)
        |%5 <- call test(%3, %1, %4)
        |end
      """.stripMargin
    )
  }

  def test_synthetic_apply(): Unit = {
    check(
      """
        |case class Test(aa: Int)
        |
        |Test(99)
      """.stripMargin,
      """
        |%0 <- call Test
        |%1 <- DfInt(99)
        |%2 <- call %0.apply(%1)
        |end
      """.stripMargin
    )
  }


  def test_apply(): Unit = {
    check(
      """
        |object Test {
        |  def apply(aa: Int): Unit = ()
        |}
        |Test(99)
      """.stripMargin,
      """
        |%0 <- call Test
        |%1 <- DfInt(99)
        |%2 <- call %0.apply(%1)
        |end
      """.stripMargin
    )
  }

  def test_explicit_apply(): Unit = {
    check(
      """
        |object Test {
        |  def apply(aa: Int): Unit = ()
        |}
        |Test.apply(99)
      """.stripMargin,
      """
        |%0 <- call Test
        |%1 <- DfInt(99)
        |%2 <- call %0.apply(%1)
        |end
      """.stripMargin
    )
  }

  def test_explicit_synthetic_apply(): Unit = {
    check(
      """
        |case class Test(aa: Int)
        |
        |Test.apply(99)
      """.stripMargin,
      """
        |%0 <- call Test
        |%1 <- DfInt(99)
        |%2 <- call %0.apply(%1)
        |end
      """.stripMargin
    )
  }

  def test_update(): Unit = {
    check(
      """
        |object Test {
        |  def update(aa: Int, s: String): Unit = ()
        |}
        |Test(11) = ""
      """.stripMargin,
      """
        |%0 <- call Test
        |%1 <- DfInt(11)
        |%2 <- DfString("")
        |%3 <- call %0.update(%1, %2)
        |end
      """.stripMargin
    )
  }

  def test_update_with_multiple_args(): Unit = {
    check(
      """
        |object Test {
        |  def update(aa: Int, bb: Int, s: String): Unit = ()
        |}
        |Test(11, 21) = ""
      """.stripMargin,
      """
        |%0 <- call Test
        |%1 <- DfInt(11)
        |%2 <- DfInt(21)
        |%3 <- DfString("")
        |%4 <- call %0.update(%1, %2, %3)
        |end
      """.stripMargin
    )
  }

  def test_explicit_update(): Unit = {
    check(
      """
        |object Test {
        |  def update(aa: Int, s: String): Unit = ()
        |}
        |Test.update(11, "")
      """.stripMargin,
      """
        |%0 <- call Test
        |%1 <- DfInt(11)
        |%2 <- DfString("")
        |%3 <- call %0.update(%1, %2)
        |end
      """.stripMargin
    )
  }

  def test_auto_tupling(): Unit = {
    check(
      """
        |def test(arg: (Int, Int)): Unit = ()
        |
        |test(1, 2)
        |""".stripMargin,
      """
        |%0 <- DfInt(1)
        |%1 <- DfInt(2)
        |%2 <- call Tuple2
        |%3 <- call %2.apply(%0, %1)
        |%4 <- call test(%3)
        |end
        |""".stripMargin
    )
  }
}
