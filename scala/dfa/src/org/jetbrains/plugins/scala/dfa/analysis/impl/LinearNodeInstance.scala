package org.jetbrains.plugins.scala.dfa.analysis.impl

import org.jetbrains.plugins.scala.dfa.analysis.NodeInstance.Controller
import org.jetbrains.plugins.scala.dfa.analysis.{NodeInstance, State}

trait LinearNodeInstance extends NodeInstance {
  override def process(state: State, controller: Controller): Unit = {
    linearProcess(state, controller)
    controller.enqueue(node.index + 1, state)
  }

  protected def linearProcess(state: State, controller: Controller): Unit
}
