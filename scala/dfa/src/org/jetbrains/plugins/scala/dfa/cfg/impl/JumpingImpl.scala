package org.jetbrains.plugins.scala.dfa
package cfg
package impl

private abstract class JumpingImpl extends NodeImpl { this: Jumping =>
  final var _targetIndex: Int = -1

  override def targetIndex: Int = _targetIndex
}
