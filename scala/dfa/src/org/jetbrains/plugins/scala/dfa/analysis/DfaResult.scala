package org.jetbrains.plugins.scala.dfa
package analysis

import org.jetbrains.plugins.scala.dfa.cfg.Graph

import scala.collection.immutable.ArraySeq

case class DfaResult[Info](graph: Graph[Info], values: ArraySeq[DfAny]) {
  assert(graph.values.size == values.size)

  def valueOf(element: Info): DfAny =
    values(graph.valueForSource(element).valueId)
}
