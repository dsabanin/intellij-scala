package org.jetbrains.plugins.scala.dfa
package cfg

import scala.collection.immutable.ArraySeq


final class Graph[SourceInfo] private[cfg](val nodes: ArraySeq[Node],
                                           val blocks: ArraySeq[Block],
                                           val arguments: ArraySeq[Argument],
                                           val valueForSource: Map[SourceInfo, Value]) {

  assert(nodes.nonEmpty)
  assert(blocks.nonEmpty)
  assert(arguments.zip(nodes).forall { case (a, n) => a eq n })
  assert(arguments.forall(_.block eq blocks.head))

  lazy val values: ArraySeq[Value] =
    nodes.collect { case value: Value => value }
      .ensuring(_.zipWithIndex.forall { case (value, idx) => value.valueId == idx })


  def apply(index: Int): Node = nodes(index)

  lazy val hasIncomingJump: Set[cfg.Node] =
    nodes.collect { case jump: cfg.Jumping => jump.target }.toSet

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
