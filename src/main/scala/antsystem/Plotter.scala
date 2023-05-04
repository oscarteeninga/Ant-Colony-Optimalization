package antsystem

import antsystem.Plotter.BenchmarkParameters
import org.joda.time.DateTime
import plotly.Plotly._
import plotly.Scatter
import plotly.element.Anchor.Center
import plotly.element.{Color, Marker}
import plotly.layout.{Axis, Layout, Legend}

trait Plotter[I <: Item, S <: Solution[I]] extends App {

  protected val title: String
  protected val xLabel: String
  protected val yLabel: String
  protected val problem: Problem[I]

  protected def run(params: BenchmarkParameters): Set[S]

  protected def run2D(params: BenchmarkParameters, z: Int): Set[S]

  val iterations = 10
  val ants = 10
  val alfa = 2
  val beta = 2

  val startOneDim = DateTime.now()

  private val oneResults = for {
    _ <- 1 to 8
    rho <- (1 to 5).map(_.toDouble / 10)
    alfa <- 0 to 2
    beta <- 1 to 3
  } yield run(BenchmarkParameters(ants, iterations, alfa, beta, rho))

  println((DateTime.now().getSecondOfDay - startOneDim.getSecondOfDay).toDouble / 360)

  val startTwoDim = DateTime.now()

  private val twoResults = for {
    _ <- 1 to 8
    rho <- (1 to 5).map(_.toDouble / 10)
    alfa <- 0 to 2
    beta <- 1 to 3
  } yield run2D(BenchmarkParameters(ants, iterations, alfa, beta, rho), ants)

  println((DateTime.now().getSecondOfDay - startTwoDim.getSecondOfDay).toDouble / 360)

  val all = twoResults.flatten ++ oneResults.flatten

  val plots = List(
    Plotter.plotting(Pareto.pareto(oneResults.flatten.toSet).head, s"Basic"),
    Plotter.plotting(Pareto.pareto(twoResults.flatten.toSet).head, s"Two-dimension"),
    Plotter.plotting(Pareto.pareto(all.toSet).head, "Pareto front").withMarker(Marker().withSize(6).withColor(Color.RGB(255, 0, 0))),
  )

  plots.plot(path = "./resources/plot/plot.html", layout = Plotter.layout(title, xLabel, yLabel))
}

object Plotter {
  case class BenchmarkParameters(ants: Int, iterations: Int, alfa: Double, beta: Double, rho: Double)

  def plotting[S <: Solution[_]](solutions: Set[S], name: String): Scatter = {
    val criteriaValues = solutions.map(_.criteriaValues.criteria.values).toList.sortBy(_.head)
    val x = criteriaValues.map(_.head)
    val y = criteriaValues.map(_.last)
    Scatter(x, y).withName(name).withMarker(Marker().withSize(10))
  }

  def layout(title: String, xLabel: String, yLabel: String): Layout = {
    val legend = Legend().withXanchor(Center).withYanchor(Center)
    Layout()
      .withTitle(title)
      .withWidth(1400)
      .withHeight(800)
      .withXaxis(Axis().withTitle(xLabel))
      .withYaxis(Axis().withTitle(yLabel))
      .withLegend(legend)
  }
}
