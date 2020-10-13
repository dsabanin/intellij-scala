package org.jetbrains.plugins.scala.dfa
package analysis
package impl

import org.jetbrains.plugins.scala.dfa.cfg.PhiValue

private final class PhiValueInstance(override val node: PhiValue) extends ValueInstance with LinearNodeInstance {
  override protected def linearProcess(state: State, controller: NodeInstance.Controller): Unit = {
    val values =
      for {
        (value, blocks) <- node.incoming.iterator
        if blocks.exists(state.isActive)
      } yield state.values(value.valueId)

    state.current = join(values)
  }
}
