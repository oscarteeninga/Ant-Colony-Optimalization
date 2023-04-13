package antsystem.tsp

import antsystem.tsp.PlotterUtils.BenchmarkParameters
import antsystem.tsp.bench.MultiTspExample
import antsystem.tsp.multi.{MultiTsp, MultiTspSolution}
import antsystem.{Pareto, Solution}
import plotly.Plotly._
import plotly._
import plotly.element.Anchor.Center
import plotly.element.{Color, Marker}
import plotly.layout._


object Plotter extends App {

  private def run(problem: MultiTsp, params: BenchmarkParameters): Set[MultiTspSolution] = {
    print(".")
    MultiTsp.resolve(params.ants, params.iterations, problem, params.alfa, params.beta, params.rho)
  }

  private def run2D(problem: MultiTsp, params: BenchmarkParameters, z: Int): Set[MultiTspSolution] = {
    print(".")
    MultiTsp.resolve2D(params.ants, params.iterations, problem, params.alfa, params.beta, params.rho, z)
  }

  val iterations = 100
  val ants = 10
  val rho = 0.25

  private val oneResults = for {
    _ <- (1 to 5)
    alfa <- (1 to 3)
    beta <- (1 to 3)
  } yield run(MultiTspExample.Random, BenchmarkParameters(ants, iterations, alfa, beta, rho))

  private val twoResults = for {
    _ <- (1 to 5)
    alfa <- (1 to 3)
    beta <- (1 to 3)
  } yield run2D(MultiTspExample.Random, BenchmarkParameters(ants, iterations, alfa, beta, rho), ants)

  val all = twoResults.flatten ++ oneResults.flatten

  val plots = List(
    PlotterUtils.plotting(Pareto.pareto(oneResults.flatten.toSet).head, s"Basic"),
    PlotterUtils.plotting(Pareto.pareto(twoResults.flatten.toSet).head, s"Two-dimension"),
    PlotterUtils.plotting(Pareto.pareto(all.toSet).head, "Pareto front").withMarker(Marker().withSize(6).withColor(Color.RGB(255, 0, 0))),
  )

  private val legend = Legend().withXanchor(Center).withYanchor(Center)
  private val layout = Layout()
    .withTitle("Tsp multiple-criteria comparison")
    .withWidth(1400)
    .withHeight(800)
    .withXaxis(Axis().withTitle("Distance [lower is better]"))
    .withYaxis(Axis().withTitle("Security [greater is better]"))
    .withLegend(legend)


  plots.plot(path = "./resources/plot/plot.html", layout = layout)
}

object PlotterUtils {
  case class BenchmarkParameters(ants: Int, iterations: Int, alfa: Double, beta: Double, rho: Double)

  def plotting[S <: Solution[_]](solutions: Set[S], name: String): Scatter = {
    val criteriaValues = solutions.map(_.criteriaValues.criteria.values).toList.sortBy(_.head)
    val x = criteriaValues.map(_.head)
    val y = criteriaValues.map(_.last)
    Scatter(x, y).withName(name).withMarker(Marker().withSize(10))
  }
}



