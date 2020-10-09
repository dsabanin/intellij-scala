package org.jetbrains.plugins.scala.lang.psi.cfg

class ExpressionTransformerTest extends TransformerTestBase {

  def test_unit(): Unit = {
    check(
      "()",
      """
        |%0 <- DfUnit.Top
        |end
      """.stripMargin
    )

    check(
      "val a = ()",
      """
        |%0 <- DfUnit.Top
        |end
      """.stripMargin
    )
  }

  def test_if_with_else(): Unit = {
    check(
      """
        |val a = true
        |if (a) {
        |  "then"
        |} else {
        |  "else"
        |}
      """.stripMargin,
      """
        |%0 <- DfTrue
        |if not %0 jump .else[2]
        |%1 <- "then"
        |jump .endIf[3]
        |.else[2]:
        |%2 <- "else"
        |.endIf[3]:
        |end
      """.stripMargin
    )

    check(
      """
        |val a = if (true) "then" else "else"
      """.stripMargin,
      """
        |%0 <- DfTrue
        |if not %0 jump .else[2]
        |%1 <- "then"
        |jump .endIf[3]
        |.else[2]:
        |%2 <- "else"
        |.endIf[3]:
        |phi %3 <- %1 | %2
        |end
      """.stripMargin
    )
  }

  def test_if_without_else(): Unit = {
    check(
      """
        |if (true) {
        |  "then"
        |}
      """.stripMargin,
      """
        |%0 <- DfTrue
        |if not %0 jump .endIf[2]
        |%1 <- "then"
        |.endIf[2]:
        |end
      """.stripMargin
    )

    check(
      """
        |val a = if (true) "then"
      """.stripMargin,
      """
        |%0 <- DfTrue
        |if not %0 jump .else[2]
        |%1 <- "then"
        |jump .endIf[3]
        |.else[2]:
        |%2 <- DfUnit.Top
        |.endIf[3]:
        |phi %3 <- %1 | %2
        |end
      """.stripMargin
    )
  }

//  def test_do_while(): Unit = {
//    check(
//      """
//        |val b = true
//        |do {
//        |  "inner"
//        |} while (b)
//        |"end"
//      """.stripMargin,
//      """
//        |b = true
//        |.LdoLoop[2]:
//        |noop "inner"
//        |if b -> .LdoLoop[2]
//        |noop "end"
//        |end
//      """.stripMargin
//    )
//  }

//  def test_while(): Unit = {
//    check(
//      """
//        |val b = true
//        |while (b) {
//        |  "inner"
//        |}
//      """.stripMargin,
//      """
//        |b = true
//        |.LwhileLoop[2]:
//        |if! b -> .LwhileExit[5]
//        |noop "inner"
//        |jmp .LwhileLoop[2]
//        |.LwhileExit[5]:
//        |end
//      """.stripMargin
//    )
//  }

  def test_block(): Unit = {
    check(
      """
        |val a = 1
        |val c = {
        |  val b = 2
        |  b
        |}
      """.stripMargin,
      """
        |%0 <- DfInt(1)
        |%1 <- DfInt(2)
        |end
      """.stripMargin
    )
  }

  def test_match_with_result(): Unit = {
    check(
      """
        |val x = 42 match {
        |  case a => 10
        |  case b => 11
        |}
        |""".stripMargin,
      """
        |%0 <- DfInt(42)
        |%1 <- DfInt(10)
        |jump .endCaseClause[2]
        |%2 <- DfInt(11)
        |.endCaseClause[2]:
        |phi %3 <- %1 | %2
        |end
        |""".stripMargin
    )
  }

  def test_match_without_result(): Unit = {
    check(
      """
        |42 match {
        |  case a => 10
        |  case b => 11
        |}
        |""".stripMargin,
      """
        |%0 <- DfInt(42)
        |%1 <- DfInt(10)
        |jump .endCaseClause[2]
        |%2 <- DfInt(11)
        |.endCaseClause[2]:
        |end
        |""".stripMargin
    )
  }

  def test_match_with_guard(): Unit = {
    check(
      """
        |42 match {
        |  case a if false => 10
        |  case b =>
        |}
        |""".stripMargin,
      """
        |%0 <- DfInt(42)
        |%1 <- DfFalse
        |if not %1 jump .endCaseClause[3]
        |%2 <- DfInt(10)
        |jump .endCaseClause[3]
        |.endCaseClause[3]:
        |end
        |""".stripMargin
    )
  }

//  def test_throw(): Unit = {
//    check(
//      """
//        |throw new AssertionError
//        |""".stripMargin,
//      """
//        |%0 <- new AssertionError
//        |%1 <- call [%0]() java.lang.AssertionError.constructor
//        |throw %1
//        |end
//        |""".stripMargin
//    )
//  }

//  def test_tuple(): Unit = {
//    check(
//      """
//        |val a = (1, 2)
//        |""".stripMargin,
//      """
//        |%0 <- Tuple2$
//        |a = call [%0](1, 2) scala.Tuple2$.apply
//        |end
//        |""".stripMargin
//    )
//
//    check(
//      """val a = true
//        |(1, a)
//        |""".stripMargin,
//      """
//        |a = true
//        |%0 <- a
//        |%1 <- Tuple2$
//        |call [%1](1, %0) scala.Tuple2$.apply
//        |end
//        |""".stripMargin
//    )
//  }


  def test_return(): Unit = check(
    """
      |return
      |return true
      |return 3
      |val a = return
      |""".stripMargin,
    """
      |end
      |%0 <- DfTrue
      |end
      |%1 <- DfInt(3)
      |end
      |end
      |%2 <- DfUnit.Top
      |end
      |""".stripMargin
  )

  /*
  todo: implement this
  def test_property_access(): Unit = {
    check(
      """
        |object Test {
        |  val test = 0
        |}
        |val a = Test.test
      """.stripMargin,
      """
        |a <- read [Test] test
        |end
      """.stripMargin
    )
  }*/
}
