package org.jetbrains.plugins.scala.dfa
package cfg

import scala.collection.immutable.ArraySeq

sealed trait Node {
  type Block = cfg.Block

  def graph: Graph[_]
  def block: Block
  def index: Int

  def asmString(showIndex: Boolean = false, showLabel: Boolean = false, indent: Boolean = false, maxIndexHint: Int = 99): String
  def labelString: String
}

sealed trait Jumping extends Node {
  def targetIndex: Int
  final def target: Node = graph(targetIndex)
  final def targetLabel: String =
    if (targetIndex < 0) "<unresolved>"
    else target.labelString
}

sealed trait Value extends Node {
  def valueId: Int

  def valueIdString: String = "%" + valueId
}

trait End extends Node

trait Constant extends Value {
  def constant: DfAny
}

trait Argument extends Value {
  def argumentName: String
  def argumentIndex: Int
}

trait PhiValue extends Value {
  def incoming: Map[Value, Seq[Block]]
}

trait Jump extends Jumping

trait JumpIfNot extends Jumping {
  def condition: Value
}

trait Call extends Value {
  def callInfo: CallInfo
  def thisValue: Option[Value]
  def arguments: ArraySeq[ArraySeq[Value]]
}

trait Instantiate extends Value {
  def instantiationInfo: InstantiationInfo
}