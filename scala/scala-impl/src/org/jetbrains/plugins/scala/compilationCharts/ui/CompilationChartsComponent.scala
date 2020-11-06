package org.jetbrains.plugins.scala.compilationCharts.ui

import com.intellij.openapi.project.Project
import com.intellij.ui.components.{JBPanel, JBScrollPane}
import com.intellij.util.ui.JBUI
import javax.swing.{JViewport, ScrollPaneConstants}
import net.miginfocom.swing.MigLayout

class CompilationChartsComponent(project: Project)
  extends JBPanel(new MigLayout("gap rel 0, ins 0")) {

  locally {
    val diagramsComponent = new DiagramsComponent(project, ActionPanel.defaultZoom)

    val diagramsScrollPane = new JBScrollPane(diagramsComponent)
    diagramsScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS)
    diagramsScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER)
    diagramsScrollPane.setBorder(JBUI.Borders.empty)
    diagramsScrollPane.getViewport.setScrollMode(JViewport.SIMPLE_SCROLL_MODE)

    val actionPanel = new ActionPanel(diagramsComponent.setZoom)
    val verticalLeftPanel = new VerticalLeftPanel(project, actionPanel.getHeight, diagramsComponent.diagramsInfo)
    val verticalRightPanel = new VerticalRightPanel(actionPanel.getHeight, diagramsComponent.diagramsInfo)

    add(verticalRightPanel, "dock east")
    add(verticalLeftPanel, "dock west")
    add(actionPanel, "al right, wrap")
    add(diagramsScrollPane, "grow, push, span")
  }
}
