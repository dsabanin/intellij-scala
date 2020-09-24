package org.jetbrains.plugins.scala.dfa
package analysis
package impl

import org.jetbrains.plugins.scala.dfa.analysis.NodeInstance.Controller

private final class ArgumentInstance(override val node: cfg.Argument) extends ValueInstance with LinearNodeInstance {
  override protected def linearProcess(state: State, controller: Controller): Unit =
    state.current = controller.arguments(node.argumentIndex)
}

