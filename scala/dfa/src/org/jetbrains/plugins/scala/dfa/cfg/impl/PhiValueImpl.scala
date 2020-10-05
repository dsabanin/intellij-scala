package org.jetbrains.plugins.scala.dfa
package cfg
package impl

private final class PhiValueImpl(incoming: Map[Value, Seq[Block]]) extends ValueImpl with PhiValue {
  override protected def asmString: String = {
    val values = incoming.keys.toSeq.sortBy(_.valueId).map(_.valueIdString)
    s"phi $valueIdString <- ${values.mkString(" | ")}"
  }
}
