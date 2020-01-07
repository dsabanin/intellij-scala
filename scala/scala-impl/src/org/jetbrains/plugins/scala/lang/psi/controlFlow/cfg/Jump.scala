package org.jetbrains.plugins.scala.lang.psi.controlFlow.cfg

import org.jetbrains.plugins.scala.dfa.{DfEntity, DfVariable}
import org.jetbrains.plugins.scala.lang.psi.controlFlow.AbstractInstructionVisitor

/**
 * Continues control flow at the target instruction
 *
 * @param targetLabel to the instruction where the control flow should be continued
 */
class Jump private[controlFlow](override val targetLabel: Label) extends JumpingInstruction {

  override def sourceEntities: Seq[DfEntity] = Seq.empty
  override def variables: Seq[DfVariable] = Seq.empty
  override def asmString: String = s"jmp $targetLabel"
  override def info: Instruction.Info = Jump
  override def accept(visitor: AbstractInstructionVisitor): Unit = visitor.visitJump(this)
}

object Jump extends Instruction.Info(
  name = "Jump",
  hasControlFlowAfter = false,
  isJump = true
)