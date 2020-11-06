package org.jetbrains.plugins.scala.compilationCharts.ui

import java.awt.Graphics2D
import java.awt.geom.Rectangle2D
import java.text.NumberFormat

import org.jetbrains.plugins.scala.compilationCharts.Memory
import org.jetbrains.plugins.scala.compilationCharts.ui.DiagramsComponent.DiagramsInfo

object Utils {

  import Constants._

  def printBorder(graphics: Graphics2D, bounds: Rectangle2D, side: Side): Unit =
    graphics.printBorder(bounds, side, InactiveTextColor, BorderStroke)

  def printVerticalPanelBorder(graphics: Graphics2D,
                               actionPanelHeight: Int,
                               diagramsInfo: DiagramsInfo,
                               side: Side): Unit = {
    val clipBounds = graphics.getClipBounds
    val progressDiagramClip = new Rectangle2D.Double(
      clipBounds.x,
      clipBounds.y + actionPanelHeight + diagramsInfo.progressDiagramY,
      clipBounds.width,
      diagramsInfo.progressDiagramHeight
    )
    printBorder(graphics, progressDiagramClip, side)
    val memoryDiagramClip = new Rectangle2D.Double(
      clipBounds.x,
      clipBounds.y + actionPanelHeight + diagramsInfo.memoryDiagramY,
      clipBounds.width,
      diagramsInfo.memoryDiagramHeight
    )
    printBorder(graphics, memoryDiagramClip, side)
  }

  def stringify(bytes: Memory): String = {
    val megabytes = bytes.toDouble / 1024 / 1024
    if (megabytes < 1000)
      s"${math.round(megabytes)} MB"
    else
      s"${MemoryGbFormatter.format(megabytes / 1024)} GB"
  }

  private val MemoryGbFormatter = {
    val formatter = NumberFormat.getNumberInstance
    formatter.setMaximumFractionDigits(2)
    formatter
  }
}
