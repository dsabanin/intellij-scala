package org.jetbrains.plugins.scala.lang.psi.impl.types
/**
* @author Ilya Sergey
*/
import com.intellij.lang.ASTNode

import org.jetbrains.plugins.scala.lang.psi._
import org.jetbrains.plugins.scala.lang.psi.impl.types._

class ScStableIdImpl( node : ASTNode ) extends ScPathImpl(node) {

   override def toString: String = "Stable Identifier: " + getText

   def getPath : ScPathImpl = {
    if (getChildren.length == 1 ) null
    else {
      val first = getChildren()(0)
      first match {
        case path : ScPathImpl => path
        case _ => null
      }
    }

  }
}