package org.jetbrains.plugins.scala.dfa
package cfg
package impl

private final class JumpImpl extends JumpingImpl with Jump {
  override protected def asmString: String = s"jump $targetLabel"
}
