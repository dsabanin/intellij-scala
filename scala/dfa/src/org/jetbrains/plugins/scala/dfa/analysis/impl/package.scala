package org.jetbrains.plugins.scala.dfa
package analysis

package object impl {
  def createNodeInstance(node: cfg.Node): NodeInstance = node match {
    case argument: cfg.Argument => new ArgumentInstance(argument)
    case constant: cfg.Constant => new ConstantInstance(constant)

    case jump: cfg.Jump => new JumpInstance(jump)
    case jump: cfg.JumpIfNot => new JumpIfNotInstance(jump)
    case end: cfg.End => new EndInstance(end)

    case call: cfg.Call => new CallInstance(call)
    case phi: cfg.PhiValue => new PhiValueInstance(phi)

    case _ => ???
  }
}
