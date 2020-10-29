package org.jetbrains.plugins.scala.dfa
package cfg

import org.jetbrains.plugins.scala.dfa.cfg.Builder.{Property, Variable}

trait Builder[SourceInfo] {
  trait ScopeInfoHolder {
    def scopeInfo: ScopeInfo
  }
  type Value
  type UnlinkedJump <: ScopeInfoHolder
  type LoopLabel
  type ScopeInfo

  def addArgument(name: String, anchor: AnyRef): (Variable, Value)
  def constant(const: DfAny): Value

  def readVariable(variable: Variable): Value
  def writeVariable(variable: Variable, value: Value): Unit

  def readProperty(base: Value, property: Property): Value
  def writeProperty(base: Value, property: Property, value: Value): Unit

  def call(callInfo: CallInfo, thisValue: Option[Value], arguments: Seq[Seq[Value]]): Value

  def ret(): Unit
  def ret(value: Value): Unit

  def jumpToFuture(): UnlinkedJump
  def jumpToFutureIfNot(cond: Value, afterBlockName: String): UnlinkedJump
  def jumpHere(blockName: String, label: Seq[UnlinkedJump]): Unit

  final def jumpHere(blockName: String, first: UnlinkedJump, rest: UnlinkedJump*): Unit =
    jumpHere(blockName, first +: rest)

  def loopJumpHere(): LoopLabel
  def jumpBack(loop: LoopLabel): Unit

  def allowDeadBlockHere(name: String, incomingScope: ScopeInfo): Unit
  def currentScopeInfo: ScopeInfo

  def addSourceInfo(value: Value, sourceInfo: SourceInfo): Unit
  def freshVariable(prefix: String = "fresh"): Variable
  def newVariable(name: String, anchor: AnyRef): Variable

  def finish(): Graph[SourceInfo]
}

object Builder {
  case class Variable(anchor: AnyRef)(override val toString: String)
  case class Property(anchor: AnyRef)(override val toString: String)

  def newBuilder[Info](): Builder[Info] =
    new impl.BuilderImpl
}