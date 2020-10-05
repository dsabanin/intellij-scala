package org.jetbrains.plugins.scala.dfa
package cfg
package impl

private final class EndImpl extends NodeImpl with End {
  override protected def asmString: String = "end"
}
