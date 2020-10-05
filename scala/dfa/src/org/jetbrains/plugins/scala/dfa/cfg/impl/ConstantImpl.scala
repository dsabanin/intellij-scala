package org.jetbrains.plugins.scala.dfa
package cfg
package impl

private final class ConstantImpl(override val constant: DfAny) extends ValueImpl with Constant {
  override protected def asmString: String = s"$valueIdString <- $constant"
}
