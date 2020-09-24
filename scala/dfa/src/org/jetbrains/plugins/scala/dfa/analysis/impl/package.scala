package org.jetbrains.plugins.scala.dfa
package analysis

package object impl {
  def createNodeInstance(node: cfg.Node): NodeInstance = node match {
    case argument: cfg.Argument => new ArgumentInstance(argument)
    case constant: cfg.Constant => new ConstantInstance(constant)
    case end: cfg.End => new EndInstance(end)
    case _ => ???
  }
}
