package org.jetbrains.plugins.scala.dfa
package analysis
package impl

private final class JumpInstance(override val node: cfg.Jump) extends NodeInstance {
  override def process(state: State, controller: NodeInstance.Controller): Unit =
    controller.enqueue(node.targetIndex, state)
}
