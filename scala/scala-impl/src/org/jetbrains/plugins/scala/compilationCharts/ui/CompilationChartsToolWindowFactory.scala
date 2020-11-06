package org.jetbrains.plugins.scala.compilationCharts.ui

import com.intellij.concurrency.JobScheduler
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.{Service, ServiceManager}
import com.intellij.openapi.project.{DumbAware, Project}
import com.intellij.openapi.wm.{ToolWindow, ToolWindowFactory}
import com.intellij.ui.content.ContentFactory
import javax.swing.JComponent
import org.jetbrains.plugins.scala.project.ProjectExt
import org.jetbrains.plugins.scala.util.ui.extensions.ComponentExt

import scala.concurrent.duration.DurationInt

final class CompilationChartsToolWindowFactory
  extends ToolWindowFactory
    with DumbAware {

  import CompilationChartsToolWindowFactory._

  override def init(toolWindow: ToolWindow): Unit =
    toolWindow.setStripeTitle("Compilation charts")

  override def isApplicable(project: Project): Boolean =
    isVisibleFor(project)

  override def shouldBeAvailable(project: Project): Boolean =
    isVisibleFor(project)

  override def createToolWindowContent(project: Project, toolWindow: ToolWindow): Unit = {
    val factory = ContentFactory.SERVICE.getInstance
    val mainComponent = initMainComponent(project)
    val content = factory.createContent(mainComponent, "Compilation", true)
    toolWindow.getContentManager.addContent(content)
    mainComponent.bindExecutionToVisibility { () =>
      JobScheduler.getScheduler.scheduleWithFixedDelay(() => refresh(project),
        0, RefreshDelay.length, RefreshDelay.unit
      )
    }
  }
}

object CompilationChartsToolWindowFactory {

  private final val RefreshDelay = 1.seconds

  def refresh(project: Project): Unit =
    MainComponentHolder.get(project).mainComponent.foreach { mainPanel =>
      refresh(mainPanel)
    }

  private def initMainComponent(project: Project): JComponent = {
    val mainComponent = new CompilationChartsComponent(project)
    MainComponentHolder.get(project).mainComponent = Some(mainComponent)
    mainComponent
  }

  private def isVisibleFor(project: Project): Boolean =
    ApplicationManager.getApplication.isInternal && project.hasScala

  @Service
  private final class MainComponentHolder {
    var mainComponent: Option[JComponent] = None
  }

  private object MainComponentHolder {

    def get(project: Project): MainComponentHolder =
      ServiceManager.getService(project, classOf[MainComponentHolder])
  }

  private def refresh(mainComponent: JComponent): Unit =
    mainComponent.repaint()
}
