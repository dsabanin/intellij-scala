package org.jetbrains.plugins.scala.dfa
package analysis
package impl

import org.jetbrains.plugins.scala.dfa.analysis.NodeInstance.Controller

private final class ConstantInstance(override val node: cfg.Constant) extends ValueInstance with LinearNodeInstance {
  override protected def linearProcess(state: State, controller: Controller): Unit =
    state.current = node.constant
}
