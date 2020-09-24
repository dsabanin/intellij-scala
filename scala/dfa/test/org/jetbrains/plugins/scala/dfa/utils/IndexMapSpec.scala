package org.jetbrains.plugins.scala.dfa
package utils

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class IndexMapSpec extends AnyFunSuite with Matchers {

  test("adding") {
    val map = IndexMap.empty[String]

    map.get(1) shouldBe None
    map.size shouldBe 0

    map += (1 -> "test")

    map.get(0) shouldBe None
    map.get(2) shouldBe None
    map(1) shouldBe "test"
    map.size shouldBe 1
  }

  test("getOrElseUpdate") {
    val map = IndexMap.empty[String]

    map.getOrElseUpdate(10, "blub") shouldBe "blub"
    map.getOrElseUpdate(10, "test") shouldBe "blub"

    map.size shouldBe 1
  }

  test("put") {
    val map = IndexMap.empty[String]

    map.put(3, "blub")

    map.size shouldBe 1
    map(3) shouldBe "blub"

    map.put(3, "test")

    map.size shouldBe 1
    map(3) shouldBe "test"

    map.put(1, "aaa")

    map.size shouldBe 2
    map(1) shouldBe "aaa"

    map.put(5, "bbb")

    map.size shouldBe 3
    map(5) shouldBe "bbb"
  }

  test("removal") {
    val map = IndexMap.empty[String]

    map.put(3, "blub")

    map.size shouldBe 1
    map.get(3) shouldBe Some("blub")

    map -= 1

    map.size shouldBe 1
    map(3) shouldBe "blub"

    map -= 3

    map.get(3) shouldBe None
    map.size shouldBe 0
  }
}
