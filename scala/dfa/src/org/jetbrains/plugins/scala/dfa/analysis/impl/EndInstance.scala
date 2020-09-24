package org.jetbrains.plugins.scala.dfa
package analysis
package impl

private final class EndInstance(override val node: cfg.End) extends NodeInstance {
  override def process(state: State, controller: NodeInstance.Controller): Unit =
    controller.addEndState(state)

}
