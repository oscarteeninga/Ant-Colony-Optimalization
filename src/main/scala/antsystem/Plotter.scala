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

  def test[A](f: (Double, Double, Double) => A): (List[A], Double) = {
    val start = DateTime.now()
    val results = for {
      _ <- 1 to 3
      rho <- List(0.1, 0.5)
      alfa <- 1 to 2
      beta <- 1 to 2
    } yield f(alfa, beta, rho)
    (results.toList, (DateTime.now().getSecondOfDay - start.getSecondOfDay).toDouble / 5*2*2*2)
  }

  private val (oneResults, oneTime) = test((alfa, beta, rho) => run(BenchmarkParameters(ants, iterations, alfa, beta, rho)))

  println(oneTime)

  private val (twoResults, twoTime) = test((alfa, beta, rho) => run2D(BenchmarkParameters(ants, iterations, alfa, beta, rho), ants))

  println(twoTime)

  val all = twoResults.flatten ++ oneResults.flatten

  val plots = List(
    Plotter.plotting(Pareto.pareto(oneResults.flatten.toSet).head, s"Basic"),
    Plotter.plotting(Pareto.pareto(twoResults.flatten.toSet).head, s"Two-dimension"),
    Plotter.plotting(Pareto.pareto(all.toSet).head, "Pareto front").withMarker(Marker().withSize(6).withColor(Color.RGB(255, 0, 0))),
  )

//  plots.plot(path = "./resources/plot/plot.html", layout = Plotter.layout(title, xLabel, yLabel))
//
//  val plots = List(
//    Plotter.plotting(oneResults.flatten.toSet, s"Basic"),
//    Plotter.plotting(twoResults.flatten.toSet, s"Two-dimension"),
//  )

  plots.plot(path = "./resources/plot/plot.html", layout = Plotter.layout(title, xLabel, yLabel))
}

trait VolumePlotter[I <: Item, S <: Solution[I]] extends App {

  protected val problem: Problem[I]
  protected val ants: Int

  def respository(parameters: BenchmarkParameters): Repository[S]

  private val (xs, ys, zs) = respository(BenchmarkParameters(25, 100, 2.0, 2.0, 0.2)).frontsByIterations.unzip3

  println(xs.mkString(","))
  println(ys.mkString(","))
  println(zs.mkString(","))
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
