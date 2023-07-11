package antsystem.tsp

import antsystem.Plotter
import antsystem.Plotter.BenchmarkParameters


object MultiTspPlotter extends Plotter[Edge, MultiTspSolution] {

  override protected lazy val title: String = "Multi travelling salesman problem"
  override protected lazy val xLabel: String = "Distance [less is better]"
  override protected lazy val yLabel: String = "Security [greater is better]"
  override protected lazy val problem: MultiTsp = KroAB100.kroAB100

  override protected def run(params: BenchmarkParameters): Set[MultiTspSolution] = {
    print(".")
    MultiTsp.resolve(params.ants, params.iterations, problem, params.alfa, params.beta, params.rho)
  }

  override protected def run2D(params: BenchmarkParameters, z: Int): Set[MultiTspSolution] = {
    print(".")
    MultiTsp.resolve2D(params.ants, params.iterations, problem, params.alfa, params.beta, params.rho, z)
  }
}



