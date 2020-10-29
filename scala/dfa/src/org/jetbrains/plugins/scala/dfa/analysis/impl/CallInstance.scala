package org.jetbrains.plugins.scala.dfa
package analysis
package impl

import NodeInstance.Controller

private final class CallInstance(override val node: cfg.Call) extends ValueInstance with LinearNodeInstance {
  override protected def linearProcess(state: State, controller: Controller): Unit =
    state.current = DfAny.Top
}
