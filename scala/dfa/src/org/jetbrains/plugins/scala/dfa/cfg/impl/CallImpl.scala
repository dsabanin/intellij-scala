package org.jetbrains.plugins.scala.dfa.cfg.impl

import org.jetbrains.plugins.scala.dfa.cfg.{Call, CallInfo, Value}

import scala.collection.immutable.ArraySeq

final private class CallImpl(override val callInfo: CallInfo,
                             override val thisValue: Option[Value],
                             override val arguments: ArraySeq[ArraySeq[Value]]) extends ValueImpl with Call {

  override protected def asmString: String = {
    val thisString = thisValue.fold("")(_.valueIdString + ".")
    val argLists = arguments.map(
      _.map(_.valueIdString)
        .mkString("(", ", ", ")")
    ).mkString
    s"$valueIdString <- call $thisString${callInfo.name}$argLists"
  }
}
