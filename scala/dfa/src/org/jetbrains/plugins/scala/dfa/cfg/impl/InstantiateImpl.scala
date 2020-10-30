package org.jetbrains.plugins.scala.dfa
package cfg
package impl

private final class InstantiateImpl(override val instantiationInfo: InstantiationInfo) extends ValueImpl with Instantiate {
  override protected def asmString: String = s"$valueIdString <- new ${instantiationInfo.typeName}"
}