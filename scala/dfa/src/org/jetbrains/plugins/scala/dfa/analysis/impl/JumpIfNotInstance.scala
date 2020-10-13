package org.jetbrains.plugins.scala.dfa
package analysis
package impl

private final class JumpIfNotInstance(override val node: cfg.JumpIfNot) extends NodeInstance {
  override def process(state: State, controller: NodeInstance.Controller): Unit = {
    val cond = state.values(node.condition.valueId)
    val condTruthiness = cond.toBoolLat
    val trueTarget = nextNodeIndex
    val falseTarget = node.targetIndex

    if (condTruthiness.canBeFalse == condTruthiness.canBeTrue) {
      controller.enqueue(trueTarget, state)
      controller.enqueue(falseTarget, state.clone())
    } else {
      val target = if (condTruthiness == BoolLat.True) trueTarget else falseTarget
      controller.enqueue(target, state)
    }
  }
}
