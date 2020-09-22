package org.jetbrains.plugins.scala.dfa
package cfg

import scala.collection.immutable.ArraySeq


final class Graph[+Info](val nodes: ArraySeq[Node], val blocks: ArraySeq[Block], val arguments: ArraySeq[Argument]) {
  assert(nodes.nonEmpty)
  assert(blocks.nonEmpty)
  assert(arguments.zip(nodes).forall { case (a, n) => a eq n })
  assert(arguments.forall(_.block eq blocks.head))

  def apply(index: Int): Node = nodes(index)

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
