package org.jetbrains.plugins.scala.dfa
package analysis
package impl

abstract class ValueInstance extends NodeInstance {
  override def node: cfg.Value

  implicit class StateExt(val state: State) {
    @inline def current: DfAny = state.values(node.valueId)
    @inline def current_=(value: DfAny): Unit = state.values(node.valueId) = value
  }
}

