package org.jetbrains.plugins.scala.lang.psi.cfg

class GenericInvocationTransformerTest extends TransformerTestBase {

  def test_generic_simple(): Unit = {
    check(
      """
        |def test[A](a: A) = a
        |
        |test(100)
      """.stripMargin,
      """
        |%0 <- DfInt(100)
        |%1 <- call test(%0)
        |end
      """.stripMargin
    )
  }

  def test_generic_multi_args(): Unit = {
    check(
      """
        |def test[A](a: A)(b: A) = ()
        |
        |test(100)(true)
      """.stripMargin,
      """
        |%0 <- DfInt(100)
        |%1 <- DfTrue
        |%2 <- call test(%0)(%1)
        |end
      """.stripMargin
    )
  }

  def test_autotupeling(): Unit = {
    check(
      """
        |def test[A](a: A) = ()
        |
        |test(1, true)
      """.stripMargin,
      """
        |%0 <- DfInt(1)
        |%1 <- DfTrue
        |%2 <- call Tuple2
        |%3 <- call %2.apply(%0, %1)
        |%4 <- call test(%3)
        |end
      """.stripMargin
    )
  }

  /*
    todo: implement this
  def test_typeclass(): Unit = {

    check(
      """
        |class TyClass[T]
        |implicit def ctx: TyClass[Int] = ()
        |
        |def test[X: TyClass](x: X) = ()
        |
        |test(100)
      """.stripMargin,
      """
        |%0 <- call () ctx
        |call (100, %0) test
        |end
      """.stripMargin
    )
  }
   */
}
