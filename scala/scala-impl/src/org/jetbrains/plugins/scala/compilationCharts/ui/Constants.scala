package org.jetbrains.plugins.scala.compilationCharts.ui

import java.awt.Color

import com.intellij.ui.components.JBPanel
import com.intellij.ui.table.JBTable
import com.intellij.util.ui.UIUtil.{FontColor, FontSize}
import com.intellij.util.ui.UIUtil

object Constants {

  final val ProgressRowHeight = new JBTable().getRowHeight * 1.5

  final val DashLength = ProgressRowHeight / 5

  final val NormalFont = UIUtil.getLabelFont(FontSize.NORMAL)
  final val SmallFont = UIUtil.getLabelFont(FontSize.SMALL)

  final val TextColor = UIUtil.getLabelFontColor(FontColor.NORMAL)
  final val InactiveTextColor = UIUtil.getInactiveTextColor
  final val TestModuleColor = new Color(98, 181, 67, 127)
  final val ProdModuleColor = new Color(64, 182, 224, 127)
  final val MemoryLineColor = new Color(231, 45, 45)

  final val NormalStroke = new LineStroke(1.0F)
  final val ThickStroke = new LineStroke(0.5F)
  final val BorderStroke = ThickStroke
}
