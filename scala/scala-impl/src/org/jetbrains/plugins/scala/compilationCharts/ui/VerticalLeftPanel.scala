package org.jetbrains.plugins.scala.compilationCharts.ui

import java.awt.geom.{Point2D, Rectangle2D}
import java.awt.{Color, Graphics, Graphics2D}

import com.intellij.openapi.project.Project
import com.intellij.ui.components.JBPanel
import org.jetbrains.plugins.scala.compilationCharts.CompileServerMetricsStateManager
import org.jetbrains.plugins.scala.compilationCharts.ui.DiagramsComponent.DiagramsInfo

class VerticalLeftPanel(project: Project,
                        getActionPanelHeight: => Int,
                        getDiagramsInfo: => DiagramsInfo)
  extends JBPanel {

  import Constants._

  override def paintComponent(g: Graphics): Unit = {
    val graphics = g.asInstanceOf[Graphics2D]
    val clipBounds = graphics.getClipBounds
    val actionPanelHeight = getActionPanelHeight
    val diagramsInfo = getDiagramsInfo
    val maxHeapSize = CompileServerMetricsStateManager.get(project).maxHeapSize

    Utils.printVerticalPanelBorder(graphics, actionPanelHeight, diagramsInfo, Side.East)

    val x = clipBounds.x + clipBounds.width - DashLength

    def printMark(y: Double): Unit =
      graphics.printHorizontalLine(new Point2D.Double(x, y), DashLength, InactiveTextColor, BorderStroke)

    for (i <- 0 to diagramsInfo.progressDiagramRowCount)
      printMark(actionPanelHeight + diagramsInfo.progressDiagramY + i * ProgressRowHeight)

    printMark(actionPanelHeight + diagramsInfo.memoryDiagramY)
    printMark(actionPanelHeight + diagramsInfo.memoryDiagramY + diagramsInfo.memoryDiagramHeight)

    def printLabel(text: String, y: Double): Double = {
      val clip = new Rectangle2D.Double(
        clipBounds.x,
        y - clipBounds.height,
        clipBounds.width - DashLength,
        2 * clipBounds.height
      )
      val rend = graphics.doInClip(clip)(_.getTextRendering(s"$text ", SmallFont, HAlign.Right, VAlign.Center))
      graphics.printText(rend, TextColor)
      rend.rect.getWidth
    }

    val parallelismTextWidth = printLabel(
      diagramsInfo.progressDiagramRowCount.toString,
      actionPanelHeight + diagramsInfo.progressDiagramY
    )
    val maxHeapSizeTextWidth = printLabel(
      Utils.stringify(maxHeapSize),
      actionPanelHeight + diagramsInfo.memoryDiagramY
    )
    val zeroHeapSizeTextWidth = printLabel(
      "0",
      actionPanelHeight + diagramsInfo.memoryDiagramY + diagramsInfo.memoryDiagramHeight
    )
    val maxTextWidth = Seq(parallelismTextWidth, maxHeapSizeTextWidth, zeroHeapSizeTextWidth).max

    val minimumSize = new Rectangle2D.Double(0, 0, maxTextWidth + DashLength, 0).getBounds.getSize
    setMinimumSize(minimumSize)
  }
}
