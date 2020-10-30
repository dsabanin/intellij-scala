package org.jetbrains.plugins.scala.lang.psi.cfg

class OperatorTransformerTest extends TransformerTestBase {
  def test_basic_infix(): Unit = {
    check(
      """
        |val a = 0 + 1
      """.stripMargin,
      """
        |%0 <- DfInt(0)
        |%1 <- DfInt(1)
        |%2 <- call %0.+(%1)
        |end
      """.stripMargin
    )
  }
  def test_basic_prefix(): Unit = {
    check(
      """
        |val b = !true
      """.stripMargin,
      """
        |%0 <- DfTrue
        |%1 <- call %0.unary_!()
        |end
      """.stripMargin
    )
  }
  def test_basic_postfix(): Unit = {
    check(
      """
        |val c = (1 toString)
      """.stripMargin,
      """
        |%0 <- DfInt(1)
        |%1 <- call %0.toString()
        |end
      """.stripMargin
    )
  }

  def test_normal_name(): Unit = {
    check(
      """
        |val a = 1
        |val b = 2
        |a compare b
      """.stripMargin,
      """
        |%0 <- DfInt(1)
        |%1 <- DfInt(2)
        |%2 <- call %0.compare(%1)
        |end
      """.stripMargin
    )
  }

  def test_changed_associativity(): Unit = {
    check(
      """
        |object Test {
        |  def -:(value: Int): Unit = ()
        |}
        |val a = 9
        |a -: Test
      """.stripMargin,
      """
        |%0 <- DfInt(9)
        |%1 <- call Test
        |%2 <- call %1.-:(%0)
        |end
      """.stripMargin
    )
  }

  def test_changed_associativity_with_defaults(): Unit = {
    check(
      """
        |val defarg = "default"
        |object Test {
        |  def -:(value: Int, default: String = defarg): Unit = ()
        |}
        |val a = 9
        |a -: Test
      """.stripMargin,
      """
        |%0 <- DfString("default")
        |%1 <- DfInt(9)
        |%2 <- call Test
        |%3 <- call %2.-:(%1, %0)
        |end
      """.stripMargin
    )
  }

  def test_infix_with_assign_on_value(): Unit = {
    check(
      """
        |class Test {
        |  def ::=(a: Any): Unit = ()
        |}
        |object Test extends Test
        |Test ::= 42
      """.stripMargin,
      """
        |%0 <- call Test
        |%1 <- DfInt(42)
        |%2 <- call %0.::=(%1)
        |end
      """.stripMargin
    )
  }

  def test_infix_with_assign_on_variable(): Unit = {
    check(
      """
        |class Test {
        |  def ::(a: Any): Test = this
        |}
        |object Test extends Test
        |var test: Test = Test
        |test ::= 32
      """.stripMargin,
      """
        |%0 <- call Test
        |%1 <- DfInt(32)
        |%2 <- call %0.::(%1)
        |end
      """.stripMargin
    )
  }

  /*
  todo: implement this
  def test_infix_with_assign_on_member_variable(): Unit = {
    check(
      """
        |object Test {
        |  var member = 3
        |}
        |Test.member += 32
      """.stripMargin,
      """
        |%0 <- Test$
        |%1 <- read [%0] member
        |%2 = call [%1](32) +
        |write [%0] member <- %2
        |end
      """.stripMargin
    )
  }*/

  /*
  todo: implement this
  def test_infix_with_assign_on_property(): Unit = {
    check(
      """
        |object Test {
        |  def prop = 3
        |  def prop_=(i: Int) = i
        |}
        |Test.prop += 42
      """.stripMargin,
      """
        |%0 <- Test$
        |%1 <- call [%0]() Test.prop
        |%2 <- call [%1](42) +
        |call [%0](%2) Test.prop_$eq
        |end
      """.stripMargin
    )
  }*/

  /*
  todo: implement this
  def test_infix_with_assign_on_indexer(): Unit = {
    check(
      """
        |object Test {
        |  def apply(idx: Int) = 3
        |  def update(idx: Int, value: Int) = value
        |}
        |val idx = 88
        |Test(idx) += 99
      """.stripMargin,
      """
        |idx = 88
        |%0 <- Test$
        |%1 <- idx
        |%2 <- call [%0](%1) Test.apply
        |%3 <- call [%2](99) +
        |call [%0](%1, %3) Test.update
        |end
      """.stripMargin
    )
  }*/
}
