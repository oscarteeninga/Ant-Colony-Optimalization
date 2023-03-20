package antsystem.knapsack

import antsystem.knapsack.bench.MultiKnapsackExample
import antsystem.knapsack.multi.{Knapsack, KnapsackSolution}
import antsystem.tsp.PlotterUtils.BenchmarkParameters
import antsystem.{Pareto, Solution}
import plotly.Plotly._
import plotly._
import plotly.element.Anchor.Center
import plotly.element.{Color, Marker}
import plotly.layout._


object Plotter extends App {

  private def run(problem: Knapsack, params: BenchmarkParameters): Set[KnapsackSolution] =
    Knapsack.resolve(params.ants, params.iterations, problem, params.alfa, params.beta, params.rho)

  private def run2D(problem: Knapsack, params: BenchmarkParameters, z: Int): Set[KnapsackSolution] =
    Knapsack.resolve2D(params.ants, params.iterations, problem, params.alfa, params.beta, params.rho, z)


  val iterations = 100
  val ants = 10
  val beta = 0.5
  val rho = 0.0
  val z = 10

  private val results = for {
    alfa <- (1 to 5).map(_ / 2.0)
  } yield {
    val oneDim = run(MultiKnapsackExample.Example, BenchmarkParameters(ants, iterations, alfa, beta, rho))
    val twoDim = run2D(MultiKnapsackExample.Example, BenchmarkParameters(ants, iterations, alfa, beta, rho), z)
    (
      List(
        PlotterUtils.plotting(oneDim, s"One-dimension [alfa = $alfa]"),
        PlotterUtils.plotting(twoDim, s"Two-dimension [alfa = $alfa]"),
      ),
      oneDim ++ twoDim
    )
  }

  val (plots, all) = results.unzip

  private val frontPareto = PlotterUtils.plotting(Pareto.pareto(all.toSet.flatten).head, "Pareto front").withMarker(Marker().withSize(3).withColor(Color.RGB(255, 0, 0)))

  private val legend = Legend().withXanchor(Center).withYanchor(Center)
  private val layout = Layout()
    .withTitle("Multiple-criteria comparison")
    .withWidth(1400)
    .withHeight(800)
    .withXaxis(Axis().withTitle("Uselessness [lower is better]"))
    .withYaxis(Axis().withTitle("Value [greater is better]"))
    .withLegend(legend)


  (plots.flatten ++ List(frontPareto)).plot(path = "./resources/plot/plot.html", layout = layout)
}

object PlotterUtils {
  case class BenchmarkParameters(ants: Int, iterations: Int, alfa: Double, beta: Double, rho: Double)

  def plotting[S <: Solution[_]](solutions: Set[S], name: String): Scatter = {
    val criteriaValues = solutions.map(_.criteriaValues.criteria.values).toList.sortBy(_.head)
    val x = criteriaValues.map(_.last)
    val y = criteriaValues.map(_.head)
    Scatter(x, y).withName(name).withMarker(Marker().withSize(5))
  }
}



