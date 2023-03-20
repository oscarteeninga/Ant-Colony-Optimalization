package antsystem.knapsack.multi

import antsystem.AntSystem
import com.typesafe.scalalogging.LazyLogging

private[knapsack] case class MultiAntSystem(problem: Knapsack, quantity: Int, alpha: Double, beta: Double, rho: Double)
  extends AntSystem[Knapsack, KnapsackSolution, Element]
    with LazyLogging {

  protected def available(solution: KnapsackSolution): Set[Element] =
    problem.items.diff(solution.items).filter(_.size < problem.size - solution.size)

  protected def emptySolution: KnapsackSolution = KnapsackSolution.Empty
}