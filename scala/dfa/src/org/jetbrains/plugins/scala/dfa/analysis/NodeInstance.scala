package org.jetbrains.plugins.scala.dfa
package analysis

import org.jetbrains.plugins.scala.dfa.analysis.NodeInstance.Controller

abstract class NodeInstance extends Cloneable {
  def node: cfg.Node
  def process(state: State, controller: Controller): Unit
  def reset(): Unit = ()
}



object NodeInstance {
  abstract class Controller {
    def arguments: Seq[DfAny]
    def enqueue(index: Int, state: State): Unit
    def addEndState(state: State): Unit
  }
}