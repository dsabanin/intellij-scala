package org.jetbrains.plugins.scala.dfa
package cfg
package impl

private final class InstantiateImpl extends ValueImpl with Instantiate {
  override protected def asmString: String = s"$valueIdString <- new"
}