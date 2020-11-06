package org.jetbrains.plugins.scala.compilationCharts.ui

import java.awt.{Color, Dimension, FlowLayout, Graphics, Graphics2D, Rectangle}

import com.intellij.icons.AllIcons
import com.intellij.openapi.actionSystem.{ActionManager, AnActionEvent, DefaultActionGroup, Presentation, Separator}
import com.intellij.openapi.actionSystem.ex.CustomComponentAction
import com.intellij.openapi.project.DumbAwareAction
import com.intellij.openapi.util.NlsActions
import com.intellij.ui.components.JBPanel
import com.intellij.util.ui.JBUI
import com.intellij.util.ui.components.BorderLayoutPanel
import javax.swing.{Icon, JComponent}
import org.jetbrains.annotations.NotNull
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.compilationCharts.ui.DiagramsComponent.Zoom

import scala.concurrent.duration.DurationInt

class ActionPanel(setZoom: Zoom => Unit)
  extends BorderLayoutPanel {

  import ActionPanel._
  import Constants._

  private var currentZoomIndex: Int = DefaultZoomIndex

  setBorder(JBUI.Borders.empty)
  addToRight(createActionToolbar())

  private def createActionToolbar(): JComponent = {
    val actionGroup = new DefaultActionGroup
    val actions = Seq(LegendAction, new Separator, ResetZoomAction, ZoomOutAction, ZoomInAction)
    actions.foreach(actionGroup.add)
    val place = classOf[CompilationChartsComponent].getSimpleName
    val toolbar = ActionManager.getInstance.createActionToolbar(place, actionGroup, true)
    val component = toolbar.getComponent
    component
  }

  private object LegendAction
    extends DumbAwareAction
      with CustomComponentAction {

    override def createCustomComponent(presentation: Presentation, place: String): JComponent = {
      val panel = new JBPanel(new FlowLayout(FlowLayout.RIGHT))
      val items = Seq(
        new LegendItem(ProdModuleColor, "Production"),
        new LegendItem(TestModuleColor, "Test"),
        new LegendItem(MemoryLineColor, "Memory")
      )
      items.foreach(panel.add)
      panel
    }

    override def actionPerformed(e: AnActionEvent): Unit = ()

    private class LegendItem(color: Color, name: String) extends JBPanel {

      override def paintComponent(g: Graphics): Unit = {
        val graphics = g.asInstanceOf[Graphics2D]
        val text = " " + name
        val rendering = graphics.getTextRendering(text, NormalFont, HAlign.Right, VAlign.Center)
        val textRect = rendering.rect.getBounds
        graphics.printText(rendering, TextColor)
        val colorY = textRect.y + textRect.height / 2 - ColorHeight / 2
        graphics.printRect(new Rectangle(0, colorY, ColorWidth, ColorHeight), color)
        setPreferredSize(new Dimension(textRect.width + ColorWidth, textRect.height))
        revalidate()
      }
    }

    private final val ColorWidth = 12
    private final val ColorHeight = 4
  }

  private abstract class BasicZoomAction(@NotNull @NlsActions.ActionText text: String,
                                         @NotNull @NlsActions.ActionDescription description: String,
                                         @NotNull icon: Icon)
    extends DumbAwareAction(text, description, icon) {

    final override def update(e: AnActionEvent): Unit =
      e.getPresentation.setEnabled(isEnabled)

    final override def actionPerformed(e: AnActionEvent): Unit = {
      currentZoomIndex = newZoomIndex
      val newZoom = AvailableZooms(currentZoomIndex)
      setZoom(newZoom)
    }

    protected def isEnabled: Boolean

    protected def newZoomIndex: Int
  }

  private object ResetZoomAction
    extends BasicZoomAction(
      ScalaBundle.message("compilation.charts.reset.zoom.action.text"),
      ScalaBundle.message("compilation.charts.reset.zoom.action.description"),
      AllIcons.General.ActualZoom) {

    override protected def isEnabled: Boolean = currentZoomIndex != DefaultZoomIndex
    override protected def newZoomIndex: Int = DefaultZoomIndex
  }

  private object ZoomOutAction
    extends BasicZoomAction(
      ScalaBundle.message("compilation.charts.zoom.out.action.text"),
      ScalaBundle.message("compilation.charts.zoom.out.action.description"),
      AllIcons.General.ZoomOut) {

    override protected def isEnabled: Boolean = currentZoomIndex > 0
    override protected def newZoomIndex: Int = currentZoomIndex - 1
  }

  private object ZoomInAction
    extends BasicZoomAction(
      ScalaBundle.message("compilation.charts.zoom.in.action.text"),
      ScalaBundle.message("compilation.charts.zoom.in.action.description"),
      AllIcons.General.ZoomIn) {

    override protected def isEnabled: Boolean = currentZoomIndex < AvailableZooms.size - 1
    override protected def newZoomIndex: Int = currentZoomIndex + 1
  }
}

object ActionPanel {

  private final val AvailableZooms = Seq(
    Zoom(5.minute, 6),
    Zoom(3.minute, 5),
    Zoom(2.minute, 5),
    Zoom(1.minute + 30.seconds, 4),
    Zoom(1.minute, 6),
    Zoom(45.seconds, 4),
    Zoom(30.seconds, 4),
    Zoom(25.seconds, 4),
    Zoom(20.seconds, 3),
    Zoom(15.seconds, 4),
    Zoom(12.seconds, 5),
    Zoom(10.seconds, 6),
    Zoom(8.seconds, 4),
    Zoom(6.seconds, 5),
    Zoom(5.seconds, 6),
    Zoom(4.seconds, 5),
    Zoom(3.seconds, 5),
    Zoom(2.seconds, 5),
    Zoom(1500.millis, 4),
    Zoom(1000.millis, 6),
    Zoom(750.millis, 4),
    Zoom(500.millis, 6),
    Zoom(250.millis, 4),
  ).sortBy(_.durationStep).reverse

  private final val DefaultZoomIndex = AvailableZooms.size / 2

  def defaultZoom: Zoom = AvailableZooms(DefaultZoomIndex)
}
