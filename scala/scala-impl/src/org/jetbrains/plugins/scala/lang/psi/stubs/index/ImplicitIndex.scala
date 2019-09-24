package org.jetbrains.plugins.scala
package lang
package psi
package stubs
package index

import com.intellij.openapi.project.Project
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.stubs.{IndexSink, StubIndex, StubIndexKey}
import com.intellij.util.Processor
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScMember
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil

import scala.collection.mutable.ArrayBuffer

trait ImplicitIndex {

  protected val indexKey: StubIndexKey[String, ScMember]

  protected def occurrence(sink: IndexSink, name: String): Unit =
    sink.occurrence(indexKey, name)

  def occurrences(sink: IndexSink, names: Array[String]): Unit =
    names.foreach(occurrence(sink, _))

  def forClassFqn(qName: String, scope: GlobalSearchScope, project: Project): Seq[ScMember] = {
    val collectProcessor = new CollectProcessor

    val withEveryPrefix =
      ScalaNamesUtil.splitName(qName)
        .tails.filter(_.nonEmpty)
        .map(_.mkString("."))

    withEveryPrefix.foreach { name =>
      StubIndex.getInstance().processElements(indexKey, name, project, scope, classOf[ScMember], collectProcessor)
    }
    collectProcessor.buffer
  }
}

private class CollectProcessor extends Processor[ScMember] {
  val buffer: ArrayBuffer[ScMember] = ArrayBuffer.empty

  def process(t: ScMember): Boolean = {
    buffer += t
    true
  }
}