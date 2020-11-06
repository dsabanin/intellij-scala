package org.jetbrains.plugins.scala.compilationCharts.ui

import java.awt.geom.{Line2D, Point2D, Rectangle2D}
import java.awt.{Color, Dimension, Graphics, Graphics2D, Point, Rectangle}

import com.intellij.ide.ui.UISettings
import com.intellij.openapi.project.Project
import com.intellij.ui.components.JBPanelWithEmptyText
import com.intellij.util.ui.UIUtil
import org.jetbrains.plugins.scala.compilationCharts.ui.DiagramsComponent.Zoom
import org.jetbrains.plugins.scala.compilationCharts.{CompilationProgressInfo, CompilationProgressState, CompilationProgressStateManager, CompileServerMemoryState, CompileServerMetricsStateManager, Memory, Timestamp}
import org.jetbrains.plugins.scala.compiler.{CompilationUnitId, ScalaCompileServerSettings}
import Constants._
import javax.swing.JViewport
import org.jetbrains.plugins.scala.extensions.invokeLater

import scala.annotation.tailrec
import scala.concurrent.duration.{Duration, DurationDouble, DurationLong, FiniteDuration}

class DiagramsComponent(project: Project,
                        defaultZoom: Zoom)
  extends JBPanelWithEmptyText {

  import DiagramsComponent._

  private var currentZoom = defaultZoom

  def setZoom(zoom: Zoom): Unit = {
    val viewport = getParent.asInstanceOf[JViewport]
    val viewRect = viewport.getViewRect
    val centerDuration = currentZoom.fromPixels(viewRect.getX + viewRect.getWidth / 2)

    currentZoom = zoom
    CompilationChartsToolWindowFactory.refresh(project)

    val newViewPosition = new Point(
      (currentZoom.toPixels(centerDuration) - viewRect.getWidth / 2).round.toInt,
      viewRect.y
    )
    // It works only with two invocations. IDK why ¯\_(ツ)_/¯
    viewport.setViewPosition(newViewPosition)
    invokeLater(viewport.setViewPosition(newViewPosition))
  }

  def diagramsInfo: DiagramsInfo = {
    val progressDiagramRowCount = getParallelism
    val diagramClips = getDiagramClips(new Rectangle2D.Double(0, 0, getWidth, getHeight), progressDiagramRowCount)
    DiagramsInfo(
      progressDiagramY = diagramClips.progressDiagram.getY,
      progressDiagramHeight = diagramClips.progressDiagram.getHeight,
      progressDiagramRowCount = progressDiagramRowCount,
      memoryDiagramY = diagramClips.memoryDiagram.getY,
      memoryDiagramHeight = diagramClips.memoryDiagram.getHeight
    )
  }

  override def paintComponent(g: Graphics): Unit = {
    val graphics = g.asInstanceOf[Graphics2D]
    UISettings.setupAntialiasing(graphics)
    val clipBounds = g.getClipBounds

    val progressState = CompilationProgressStateManager.get(project)
    val metricsState = CompileServerMetricsStateManager.get(project)

    val diagrams = calculateDiagrams(progressState, metricsState)
    val Diagrams(progressDiagram, memoryDiagram, progressTime) = diagrams
    val preferredWidth = math.max(
      currentZoom.toPixels(progressTime + durationAhead),
      clipBounds.width
    )
    val clips = getDiagramClips(clipBounds, progressDiagram.rowCount)

    graphics.doInClip(clips.progressDiagram)(printProgressDiagram(_, progressDiagram, progressTime, preferredWidth))
    graphics.doInClip(clips.delimiter)(printDelimiter)
    graphics.doInClip(clips.memoryDiagram)(printMemoryDiagram(_, memoryDiagram, preferredWidth, progressTime))
    graphics.doInClip(clips.durationAxis)(printDurationAxis(_, preferredWidth))

    val preferredSize = {
      val preferredHeight = clips.durationAxis.getY + clips.durationAxis.getHeight - clips.progressDiagram.getY
      new Rectangle2D.Double(0, 0, preferredWidth, preferredHeight).getBounds.getSize
    }
    setPreferredSize(preferredSize)
    revalidate()
  }

  private def getDiagramClips(clipBounds: Rectangle2D, progressDiagramRowCount: Int): DiagramClips = {
    def nextClip(height: Double, prevClip: Rectangle2D): Rectangle2D.Double =
      new Rectangle2D.Double(clipBounds.getX, prevClip.getY + prevClip.getHeight, clipBounds.getWidth, height)

    val progressDiagramHeight = progressDiagramRowCount * ProgressRowHeight
    val progressDiagramClip = new Rectangle2D.Double(clipBounds.getX, clipBounds.getY, clipBounds.getWidth, progressDiagramHeight)
    val delimiterClip = nextClip(DelimiterHeight, progressDiagramClip)
    val memoryDiagramY = delimiterClip.y + delimiterClip.height
    val memoryDiagramHeight = math.max(
      clipBounds.getHeight - memoryDiagramY - DurationAxisHeight,
      MinMemoryDiagramHeight
    )
    val memoryDiagramClip = nextClip(memoryDiagramHeight, delimiterClip)
    val durationAxisClip = nextClip(DurationAxisHeight, memoryDiagramClip)
    DiagramClips(
      progressDiagram = progressDiagramClip,
      delimiter = delimiterClip,
      memoryDiagram = memoryDiagramClip,
      durationAxis = durationAxisClip
    )
  }

  private def printProgressDiagram(graphics: Graphics2D,
                                   progressDiagram: ProgressDiagram,
                                   progressTime: FiniteDuration,
                                   preferredWidth: Double): Unit =
    printDiagram(graphics, preferredWidth, progressTime) { g =>
      val clipBounds = g.getClipBounds
      val segmentBorderThickness = BorderStroke.thickness

      def printSegment(segment: Segment, row: Int): Unit = {
        val Segment(CompilationUnitId(moduleName, testScope), from, to, progress) = segment
        val x = currentZoom.toPixels(from)
        val segmentClip = new Rectangle2D.Double(
          x,
          clipBounds.y + clipBounds.height - ProgressRowHeight * row + segmentBorderThickness,
          math.max(currentZoom.toPixels(to) - x - segmentBorderThickness, segmentBorderThickness),
          ProgressRowHeight - segmentBorderThickness
        )
        val color = if (testScope) TestModuleColor else ProdModuleColor
        g.doInClip(clipBounds & segmentClip)(_.printRect(segmentClip, color))
        val text = s" $moduleName"
        val textRendering = g.doInClip(segmentClip)(_.getReducedTextRendering(text, NormalFont, HAlign.Left))
        g.doInClip(clipBounds & textRendering.rect)(_.printText(textRendering, TextColor))
        if (progress < 1.0) {
          val textClipX = segmentClip.x + segmentClip.width + segmentBorderThickness
          val textClip = new Rectangle2D.Double(
            textClipX,
            segmentClip.y,
            preferredWidth - textClipX,
            segmentClip.height
          )
          val text = s" ${(progress * 100).round}%"
          val rendering = g.doInClip(textClip)(_.getTextRendering(text, NormalFont, HAlign.Left, VAlign.Center))
          g.doInClip(clipBounds & rendering.rect)(_.printText(rendering, TextColor))
        }
      }

      progressDiagram.segmentGroups.zipWithIndex.foreach { case (group, row) =>
        group.foreach(printSegment(_, row + 1))
      }
    }

  private def printDelimiter(graphics: Graphics2D): Unit = {
    val clipBounds = graphics.getClipBounds
    graphics.printRect(clipBounds, panelColor)
    Utils.printBorder(graphics, clipBounds, Side.North)
  }

  private def printMemoryDiagram(graphics: Graphics2D,
                                 memoryDiagram: MemoryDiagram,
                                 preferredWidth: Double,
                                 progressTime: FiniteDuration): Unit =
    printDiagram(graphics, preferredWidth, progressTime) { g =>
      val clipBounds = g.getClipBounds
      val MemoryDiagram(points, maxMemory) = memoryDiagram

      def getExtraPoints(edgePoint: Option[MemoryPoint],
                         extraPointTime: FiniteDuration,
                         firstPoint: Boolean): Seq[MemoryPoint] = edgePoint match {
        case Some(MemoryPoint(`extraPointTime`, 0)) =>
          Seq.empty
        case Some(MemoryPoint(`extraPointTime`, _)) | None =>
          Seq(MemoryPoint(extraPointTime, 0L))
        case Some(point) =>
          val extraPoints = Seq(point.copy(time = extraPointTime), MemoryPoint(extraPointTime, 0L))
          if (firstPoint) extraPoints.reverse else extraPoints
      }

      def toPlotPoint(memoryPoint: MemoryPoint): (Double, Double) = {
        val MemoryPoint(time, memory) = memoryPoint
        val x = currentZoom.toPixels(time)
        val y = clipBounds.y + clipBounds.height - (memory.toDouble / maxMemory * clipBounds.height)
        (x, y)
      }

      val leftExtraPoints = getExtraPoints(points.headOption, Duration.Zero, firstPoint = true)
      val rightExtraPoints = getExtraPoints(points.lastOption, progressTime, firstPoint = false)
      val allPoints = leftExtraPoints ++ points ++ rightExtraPoints
      val plotPoints = allPoints.map(toPlotPoint)
      val polygon = new Polygon2D(plotPoints)
      g.printPolygon(polygon, MemoryFillColor)

      if (plotPoints.size >= 2)
        plotPoints.sliding(2).foreach { case Seq((x1, y1), (x2, y2)) =>
          if (x1 != x2)
            g.printLine(new Line2D.Double(x1, y1, x2, y2), MemoryLineColor, NormalStroke)
        }
      points.lastOption.foreach { point =>
        val x = currentZoom.toPixels(progressTime)
        val (_, y) = toPlotPoint(point)
        val clip = new Rectangle2D.Double(x, y - clipBounds.height, clipBounds.width, 2 * clipBounds.height)
        val text = " " + Utils.stringify(point.memory)
        val rendering = g.doInClip(clip) { g =>
          val rendering = g.getTextRendering(text, NormalFont, HAlign.Left, VAlign.Center)
          val rect = rendering.rect
          lazy val topDelta = clipBounds.y - rect.getY
          lazy val bottomDelta = clipBounds.y + clipBounds.height - rect.getY - rect.getHeight
          val deltaY = if (topDelta > 0)
            topDelta
          else if (bottomDelta < 0)
            bottomDelta
          else
            0
          rendering.translate(rendering.x, rendering.y + deltaY)
        }
        g.doInClip(clipBounds & rendering.rect)(_.printText(rendering, TextColor))
      }
    }

  private def printDurationAxis(graphics: Graphics2D, preferredWidth: Double): Unit = {
    val clipBounds = graphics.getClipBounds

    graphics.printRect(clipBounds, panelColor)
    Utils.printBorder(graphics, clipBounds, Side.North)
    durationXIterator(preferredWidth).zipWithIndex.foreach { case (x, i) =>
      val point = new Point2D.Double(x, clipBounds.y)
      if (i % currentZoom.durationLabelPeriod == 0) {
        graphics.printVerticalLine(point, LongDashLength, InactiveTextColor, ThickStroke)
        val text = " " + stringify(i * currentZoom.durationStep)
        val textClip = new Rectangle2D.Double(point.x, clipBounds.y, clipBounds.width, clipBounds.height)
        val rendering = graphics.doInClip(textClip)(_.getTextRendering(text, SmallFont, HAlign.Left, VAlign.Top))
        val fixedRendering = rendering.translate(rendering.x, rendering.y + rendering.rect.getHeight / 4)
        graphics.doInClip(clipBounds & fixedRendering.rect)(_.printText(fixedRendering, TextColor))
      } else {
        graphics.printVerticalLine(point, DashLength, InactiveTextColor, ThickStroke)
      }
    }
  }

  private def printDiagram(graphics: Graphics2D,
                           preferredWidth: Double,
                           progressTime: FiniteDuration)
                          (print: Graphics2D => Unit): Unit = {
    val clipBounds = graphics.getClipBounds
    graphics.printRect(clipBounds, BackgroundColor)

    durationXIterator(preferredWidth).zipWithIndex.foreach { case (x, i) =>
      if (i % currentZoom.durationLabelPeriod == 0) {
        val point = new Point2D.Double(x, clipBounds.y)
        graphics.printVerticalLine(point, clipBounds.height, InactiveTextColor, DashedStroke)
      }
    }
    val progressLinePoint = currentZoom.toPixels(progressTime)
    val linePoint = new Point2D.Double(progressLinePoint, clipBounds.y)
    graphics.printVerticalLine(linePoint, clipBounds.height, TextColor, ThickStroke)

    print(graphics)

    Utils.printBorder(graphics, clipBounds, Side.North)
  }

  private def durationXIterator(preferredWidth: Double): Iterator[Double] = {
    val step = currentZoom.toPixels(currentZoom.durationStep)
    Iterator.iterate(0.0)(_ + step).takeWhile(_ <= preferredWidth)
  }

  private def durationAhead: FiniteDuration =
    currentZoom.durationStep * currentZoom.durationLabelPeriod

  private def panelColor: Color =
    getBackground
}

object DiagramsComponent {

  final case class Zoom(durationStep: FiniteDuration,
                        durationLabelPeriod: Int) {

    private lazy val scale = 5.5e7 / durationStep.toNanos

    def toPixels(duration: FiniteDuration): Double = scale * duration.toMillis

    def fromPixels(pixels: Double): FiniteDuration = (pixels / scale).millis
  }

  final case class DiagramsInfo(progressDiagramY: Double,
                                progressDiagramHeight: Double,
                                progressDiagramRowCount: Int,
                                memoryDiagramY: Double,
                                memoryDiagramHeight: Double)

  private def getParallelism: Int = {
    val settings = ScalaCompileServerSettings.getInstance
    if (settings.COMPILE_SERVER_PARALLEL_COMPILATION) settings.COMPILE_SERVER_PARALLELISM else 1
  }

  private final case class DiagramClips(progressDiagram: Rectangle2D,
                                        delimiter: Rectangle2D,
                                        memoryDiagram: Rectangle2D,
                                        durationAxis: Rectangle2D)

  private final val MemoryFillColor = new Color(231, 45, 45, 13)
  private final val BackgroundColor = UIUtil.getTreeBackground

  private final val MinMemoryDiagramHeight = ProgressRowHeight * 3
  private final val DelimiterHeight = ProgressRowHeight
  private final val DurationAxisHeight = ProgressRowHeight
  private final val LongDashLength = DashLength * 2

  private final val DashedStroke = new LineStroke(0.5F, dashLength = Some((ProgressRowHeight / 5).toFloat))

  private final case class Diagrams(progressDiagram: ProgressDiagram,
                                    memoryDiagram: MemoryDiagram,
                                    progressTime: FiniteDuration)

  private final case class ProgressDiagram(segmentGroups: Seq[Seq[Segment]],
                                           rowCount: Int)

  private final case class Segment(unitId: CompilationUnitId,
                                   from: FiniteDuration,
                                   to: FiniteDuration,
                                   progress: Double)

  private final case class MemoryDiagram(points: Seq[MemoryPoint], maxMemory: Memory)

  private final case class MemoryPoint(time: FiniteDuration, memory: Memory)

  private def calculateDiagrams(progressState: CompilationProgressState,
                                metricsState: CompileServerMemoryState): Diagrams = {
    val progressRowCount = getParallelism
    val result = for {
      (minTimestamp, maxTimestamp) <- getMinMaxTimestamps(progressState)
      progressDiagram = calculateProgressDiagram(progressState, progressRowCount, minTimestamp, maxTimestamp)
      progressTime <- progressDiagram.segmentGroups.flatten.map(_.to).maxOption
      memoryDiagram = calculateMemoryDiagram(metricsState, minTimestamp, maxTimestamp)
    } yield Diagrams(
      progressDiagram = progressDiagram,
      memoryDiagram = memoryDiagram,
      progressTime = progressTime
    )
    result.getOrElse(Diagrams(
      ProgressDiagram(Seq.empty, progressRowCount),
      MemoryDiagram(Seq.empty, metricsState.maxHeapSize),
      Duration.Zero
    ))
  }

  private def getMinMaxTimestamps(progressState: CompilationProgressState): Option[(Timestamp, Timestamp)] = {
    val timestamps = progressState.values.flatMap { info =>
      Seq(info.startTime, info.updateTime) ++ info.finishTime.toSeq
    }
    for {
      min <- timestamps.minOption
      max <- timestamps.maxOption
    } yield (min, max)
  }

  private def calculateProgressDiagram(progressState: CompilationProgressState,
                                       rowCount: Int,
                                       minTimestamp: Timestamp,
                                       maxTimestamp: Timestamp): ProgressDiagram = {
    def groupSegments(segments: Seq[Segment]): Seq[Seq[Segment]] = {
      @tailrec
      def rec(groups: Seq[Seq[Segment]],
              segments: Seq[Segment]): Seq[Seq[Segment]] = segments match {
        case Seq() => groups
        case Seq(interval, remainIntervals@_*) => rec(insert(groups, interval), remainIntervals)
      }

      def insert(groups: Seq[Seq[Segment]],
                 segment: Segment): Seq[Seq[Segment]] = groups match {
        case Seq() =>
          Seq(Seq(segment))
        case Seq(group, remainGroups@_*) =>
          if (group.last.to < segment.from)
            (group :+ segment) +: remainGroups
          else
            group +: insert(remainGroups, segment)
      }

      rec(Seq.empty, segments)
    }

    val sortedState = progressState.toSeq.sortBy(_._2.startTime)
    val segments = sortedState.flatMap { case (unitId, CompilationProgressInfo(startTime, finishTime, _, progress)) =>
      val from = (startTime - minTimestamp).nanos
      val to = (finishTime.getOrElse(maxTimestamp) - minTimestamp).nanos
      if (from.length >= 0 && to.length >= 0)
        Some(Segment(
          unitId = unitId,
          from = from,
          to = to,
          progress = progress
        ))
      else
        None
    }
    ProgressDiagram(groupSegments(segments), rowCount)
  }

  private def calculateMemoryDiagram(metricsState: CompileServerMemoryState,
                                     minTimestamp: Timestamp,
                                     maxTimestamp: Timestamp): MemoryDiagram = {
    val points = metricsState.heapUsed.map { case (timestamp, memory) =>
      val fixedTimestamp = if (timestamp < minTimestamp)
        minTimestamp
      else if (timestamp > maxTimestamp)
        maxTimestamp
      else
        timestamp
      fixedTimestamp -> memory
    }.map { case (timestamp, memory) =>
      val time = (timestamp - minTimestamp).nanos
      MemoryPoint(time, memory)
    }.toSeq.sortBy(_.time)

    val maxMemory = metricsState.maxHeapSize
    MemoryDiagram(points, maxMemory)
  }

  private def stringify(duration: FiniteDuration): String = {
    val minutes = duration.toMinutes
    val seconds = duration.toSeconds % 60
    val minutesStr = Option(minutes).filter(_ > 0).map(_.toString + "m")
    val secondsStr = Option(seconds).filter(_ > 0).map(_.toString + "s")
    val result = Seq(minutesStr, secondsStr).flatten.mkString(" ")
    if (result.nonEmpty) result else "0s"
  }
}
