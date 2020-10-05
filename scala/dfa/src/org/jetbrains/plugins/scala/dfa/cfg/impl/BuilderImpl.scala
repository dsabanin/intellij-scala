package org.jetbrains.plugins.scala.dfa
package cfg
package impl

import org.jetbrains.plugins.scala.dfa.cfg.Builder.{Property, Variable}
import org.jetbrains.plugins.scala.dfa.utils.BuilderWithSize

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

private[cfg] class BuilderImpl[SourceInfo] extends Builder[SourceInfo] {
  override type Value = cfg.Value
  override type UnlinkedJump = UnlinkedJumpImpl
  override type LoopLabel = LoopLabelImpl

  private type NodeImpl = impl.NodeImpl with Node
  private type JumpingImpl = impl.JumpingImpl with Jumping
  private type ArgumentImpl = impl.ArgumentImpl
  private type Block = impl.BlockImpl

  private class Scope(val block: Block, var variables: Map[Variable, Value])

  private val sourceMappingBuilder = Map.newBuilder[SourceInfo, Value]
  private val nodesBuilder = BuilderWithSize.newBuilder[NodeImpl](ArraySeq)
  private val blocksBuilder = BuilderWithSize.newBuilder[Block](ArraySeq)
  private val blockOutgoings = mutable.Map.empty[Block, mutable.Builder[Block, ArraySeq[Block]]]


  // Normally there should be scope whenever there is a block and vice versa.
  // But there is this weird state where we insert phi nodes to build the scope.
  // Tn that state there is a block but no a scope.
  private var curMaybeBlock = Option.empty[Block]
  private var curMaybeScope = Option.empty[Scope]

  locally {
    startBlock("<main>", Seq.empty)
  }

  private def currentBlock: Block = curMaybeBlock.get
  private def currentScope: Scope =
    curMaybeScope.get.ensuring(curMaybeScope.nonEmpty, "Whenever there is a scope, there should be a block")

  private def startBlock(name: String, incomingScopes: Seq[Scope]): Unit = {
    assert(curMaybeBlock.isEmpty)
    assert(curMaybeScope.isEmpty)
    val block = new Block(
      name,
      index = blocksBuilder.elementsAdded,
      nodeBegin = nodesBuilder.elementsAdded,
      incoming = incomingScopes.iterator.map(_.block).to(ArraySeq)
    )
    curMaybeBlock = Some(block)

    for (from <- incomingScopes) {
      val builder = blockOutgoings.getOrElseUpdate(from.block, ArraySeq.newBuilder)
      builder += block
    }

    // build scope
    val newVariables = unifyVariables(incomingScopes)

    curMaybeScope = Some(new Scope(block, newVariables))
  }

  private def unifyVariables(incomingScope: Seq[Scope]): Map[Variable, Value] = {
    val builder = Map.newBuilder[Variable, Value]

    val lifeVariables = incomingScope
      .map(_.variables.keySet)
      .reduceOption(_ & _)
      .getOrElse(Set.empty)

    for (variable <- lifeVariables) {
      val incomingValues = incomingScope.groupMap(_.variables(variable))(_.block)
      val phi = addNode(new PhiValueImpl(incomingValues))

      builder += (variable -> phi)
    }

    builder.result()
  }

  private def closeBlock(): Scope = {
    val scope = currentScope
    val block = scope.block
    block._endIndex = nodesBuilder.elementsAdded

    blocksBuilder += block
    curMaybeBlock = None
    curMaybeScope = None
    scope
  }

  private def closeBlockIfNeeded(): Option[Scope] =
    curMaybeBlock.map { _ => closeBlock() }

  private var nextValueId = 0
  private def newValueId(): Int = {
    val next = nextValueId
    nextValueId += 1
    next
  }

  private def addNode(node: NodeImpl): node.type = {
    node._index = nodesBuilder.elementsAdded
    node._block = currentBlock

    node match {
      case value: ValueImpl =>
        value._valueId = newValueId()
      case _ =>
    }

    nodesBuilder += node
    node
  }

  private val argumentsBuilder = BuilderWithSize.newBuilder[ArgumentImpl](ArraySeq)
  override def addArgument(name: String, anchor: AnyRef): (Variable, Value) = {
    val argumentsAdded = argumentsBuilder.elementsAdded
    assert(argumentsAdded == nodesBuilder.elementsAdded, "Cannot add arguments after having added other nodes")

    val variable = newVariable(name, anchor)
    val argNode = addNode(new ArgumentImpl(name))
    assert(argNode.valueId == argumentsAdded)
    writeVariable(variable, argNode)
    argumentsBuilder += argNode

    (variable, argNode)
  }

  override def constant(const: DfAny): Value =
    addNode(new ConstantImpl(const))

  override def readVariable(variable: Variable): Value =
    currentScope.variables(variable)
  override def writeVariable(variable: Variable, value: Value): Unit =
    currentScope.variables += variable -> value

  override def readProperty(base: Value, property: Property): Value = ???
  override def writeProperty(base: Value, property: Property, value: Value): Unit = ???

  /***** Forward jumps ****/
  private val unlinkedJumps = mutable.Set.empty[UnlinkedJump]
  private def addForwardJump(jump: JumpingImpl, nameAfterBlock: Option[String] = None): UnlinkedJump = {
    addNode(jump)
    val prevBlockInfo = closeBlock()
    val unlinkedJump = new UnlinkedJumpImpl(jump, prevBlockInfo)
    unlinkedJumps += unlinkedJump

    nameAfterBlock.foreach(startBlock(_, Seq(prevBlockInfo)))

    unlinkedJump
  }

  override def jumpToFuture(): UnlinkedJump = addForwardJump(new JumpImpl)
  override def jumpToFutureIfNot(cond: Value, afterBlockName: String): UnlinkedJump =
    addForwardJump(new JumpIfNotImpl(cond), Some(afterBlockName))

  override def jumpHere(blockName: String, labels: Seq[UnlinkedJump]): Unit = {
    val prevBlockInfo = closeBlockIfNeeded()
    val targetIndex = nodesBuilder.elementsAdded

    labels.foreach(_.finish(targetIndex))

    startBlock(blockName, prevBlockInfo.toSeq ++ labels.map(_.blockInfo))
  }

  class UnlinkedJumpImpl(private[BuilderImpl] val jumping: JumpingImpl,
                         private[BuilderImpl] val blockInfo: Scope) {
    private[BuilderImpl] def finish(targetIndex: Int): Unit = {
      assert(unlinkedJumps contains this)
      unlinkedJumps -= this
      jumping._targetIndex = targetIndex
    }
  }

  /***** Backward jumps *****/
  override def loopJumpHere(): LoopLabelImpl = ???
  override def jumpBack(loop: LoopLabelImpl): Unit = ???

  class LoopLabelImpl {

  }

  /***** Additional stuff *****/
  override def addSourceInfo(value: Value, sourceInfo: SourceInfo): Unit =
    sourceMappingBuilder += sourceInfo -> value

  private var nextFreshVarId = 0
  override def freshVariable(prefix: String): Variable =
    try Variable(new AnyRef)(prefix + "#" + nextFreshVarId)
    finally nextFreshVarId += 1

  private val variableAnchors = mutable.Set.empty[AnyRef]
  override def newVariable(name: String, anchor: AnyRef): Variable = {
    assert(!variableAnchors.contains(anchor))
    variableAnchors += anchor

    Variable(anchor)(name)
  }

  /***** Create Graph *****/
  override def finish(): Graph[SourceInfo] = {
    addNode(new EndImpl)
    closeBlock()

    assert(unlinkedJumps.isEmpty, "Unlinked labels: " + unlinkedJumps.iterator.map(_.jumping.index).mkString(", "))

    val nodes = nodesBuilder.result()
    val blocks = blocksBuilder.result()
    val arguments = argumentsBuilder.result()
    val valueForSource = sourceMappingBuilder.result()
    val graph = new Graph[SourceInfo](nodes, blocks, arguments, valueForSource)
    blocks.foreach(_._graph = graph)

    // build outgoings for blocks
    for ((from, toBuilder) <- blockOutgoings) {
      from._outgoing = toBuilder.result()
    }


    {
      // check if blocks have correct boundaries
      assert(blocks.head.nodeBegin == 0)
      assert(blocks.size == 1 || blocks.sliding(2).forall { case ArraySeq(a, b) => a.nodeEnd == b.nodeBegin })
      assert(blocks.last.nodeEnd == graph.nodes.size)

      // check indices
      nodes.zipWithIndex.foreach { case (node, idx) => assert(node.index == idx) }
      blocks.zipWithIndex.foreach { case (block, idx) => assert(block.index == idx) }

      // sanity checks
      nodes.foreach(_.sanityCheck())
      blocks.foreach(_.sanityCheck())
    }

    graph
  }
}