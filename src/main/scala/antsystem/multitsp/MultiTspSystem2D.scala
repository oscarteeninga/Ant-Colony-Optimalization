package antsystem.multitsp

import com.typesafe.scalalogging.LazyLogging


class MultiTspSystem2D(problem: MultiTsp, quantity: Int, alpha: Double, beta: Double, rho: Double)
  extends MultiTspSystem(problem, quantity, alpha, beta, rho)
    with LazyLogging {
  
  private val tauZero: Double = 1.0

  var tau2D: Map[Edge, List[Double]] = problem.items.map(_ -> problem.edges.edges.map(_ => tauZero).toList).toMap

  override def runPareto(iterations: Int): Set[MultiTspSolution] =
    pareto {
      (0 to iterations).flatMap { it =>
        val solutions = ants.map(_.explore)
        val fronts = pareto(solutions)
        update(fronts)
        fronts.head
      }.toSet
    }.head

  override def probabilities(edges: Set[Edge], solution: MultiTspSolution): Map[Edge, Double] = {
    val dividers = edges.map(edge => edge -> Math.pow(tau(edge), alpha)).toMap
    val sum = dividers.values.sum
    dividers.map { case (edge, value) => edge -> value / sum }
  }

  override def update(paretoFronts: List[Set[MultiTspSolution]]): Unit = {
    val paretoFrontsValues = paretoFronts.zipWithIndex.map { case (front, index) => front -> (10 - index) / 10 }
    paretoFrontsValues.foreach { case (solutions, factor) =>
      solutions.foreach { solution =>
        solution.edges.foreach(edge => tau = tau.updated(edge, tau(edge) + 1 * factor))
      }
    }
    tau = tau.map { case (edge, pheromone) => edge -> pheromone * (1 - rho) }
  }
}