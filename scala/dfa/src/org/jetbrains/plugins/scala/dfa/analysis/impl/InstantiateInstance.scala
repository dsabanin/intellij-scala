package org.jetbrains.plugins.scala.dfa
package analysis
package impl

private final class InstantiateInstance(override val node: cfg.Instantiate) extends ValueInstance with LinearNodeInstance {
  // use one instance per new call
  private val inst = new DfAnyRef.Concrete

  override protected def linearProcess(state: State, controller: NodeInstance.Controller): Unit =
    state.current = inst
}

