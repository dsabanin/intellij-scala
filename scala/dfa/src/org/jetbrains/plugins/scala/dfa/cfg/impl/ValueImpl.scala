package org.jetbrains.plugins.scala.dfa
package cfg
package impl

private abstract class ValueImpl extends NodeImpl { this: Value =>
  final var _valueId: Int = -1

  override def valueId: Int = _valueId
}
