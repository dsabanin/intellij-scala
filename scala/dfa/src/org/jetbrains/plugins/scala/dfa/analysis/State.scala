package org.jetbrains.plugins.scala.dfa
package analysis

class State(val blockActive: Array[Boolean], val values: Array[DfAny]) extends Cloneable {
  def isActive(block: cfg.Block): Boolean = blockActive(block.index)
  def isActive(node: cfg.Node): Boolean = isActive(node.block)
  def isActive(instance: NodeInstance): Boolean = isActive(instance.node)

  override def clone(): State = new State(blockActive.clone(), values.clone())
}

object State {
  def from(graph: cfg.Graph[_]): State = {
    val blockActive = new Array[Boolean](graph.blocks.size)
    assert(blockActive.forall(_ == false))
    blockActive(0) = true

    val values = Array.fill[DfAny](graph.values.size)(DfNothing)

    new State(blockActive, values)
  }
}