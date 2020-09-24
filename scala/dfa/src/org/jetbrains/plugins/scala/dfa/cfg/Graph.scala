package org.jetbrains.plugins.scala.dfa
package cfg

import scala.collection.immutable.ArraySeq


final class Graph[Info](val nodes: ArraySeq[Node], val blocks: ArraySeq[Block], val arguments: ArraySeq[Argument]) {
  assert(nodes.nonEmpty)
  assert(blocks.nonEmpty)
  assert(arguments.zip(nodes).forall { case (a, n) => a eq n })
  assert(arguments.forall(_.block eq blocks.head))

  val values: ArraySeq[Value] =
    nodes.collect { case value: Value => value }
      .ensuring(_.zipWithIndex.forall { case (value, idx) => value.valueId == idx })


  def apply(index: Int): Node = nodes(index)

  def nodesForSource(info: Info): Seq[Node] = nodes.filter(_.sourceInfo.contains(info))
  def valuesForSource(info: Info): Seq[Value] = values.filter(_.sourceInfo.contains(info))
  def valueForSource(info: Info): Value = {
    val values = valuesForSource(info)
    assert(values.nonEmpty)
    assert(values.size == 1)
    values.head
  }

  lazy val hasIncomingJump: Set[Node] =
    nodes.collect { case jump: Jumping => jump.target }.toSet

  def asmText(showIndices: Boolean = false, indent: Boolean = false): String = {
    val builder = new StringBuilder
    for (node <- nodes) {
      builder ++= node.asmString(
        showIndex = showIndices,
        showLabel = hasIncomingJump(node),
        indent = indent,
        maxIndexHint = nodes.size
      )
      builder += '\n'
    }

    builder.result()
  }
}
