package antsystem.knapsack

import antsystem.Plotter
import antsystem.Plotter.BenchmarkParameters


object KnapsackPlotter extends Plotter[Element, KnapsackSolution] {

  override protected lazy val title: String = "Knapsack"
  override protected lazy val xLabel: String = "Uselessness [lower is better]"
  override protected lazy val yLabel: String = "Value [greater is better]"
  override protected lazy val problem: Knapsack = MultiKnapsackExample.Example

  override protected def run(params: BenchmarkParameters): Set[KnapsackSolution] = {
    print(".")
    Knapsack.resolve(params.ants, params.iterations, problem, params.alfa, params.beta, params.rho)
  }

  override protected def run2D(params: BenchmarkParameters, z: Int): Set[KnapsackSolution] = {
    print(".")
    Knapsack.resolve2D(params.ants, params.iterations, problem, params.alfa, params.beta, params.rho, z)
  }
}



