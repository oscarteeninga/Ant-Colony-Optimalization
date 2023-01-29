package antsystem.multitsp

import antsystem.AntSystem
import com.typesafe.scalalogging.LazyLogging


case class MultiTspSystem(problem: MultiTsp, quantity: Int, alpha: Double, beta: Double, rho: Double)
  extends AntSystem[MultiTsp, MultiTspSolution, Edge]
    with LazyLogging {
  
  private val tauZero: Double = 1.0

  var tau: Map[Edge, Double] = problem.items.map(_ -> tauZero).toMap

  def runPareto(iterations: Int): Set[MultiTspSolution] =
    pareto {
      (0 to iterations).flatMap { it =>
        val solutions = ants.map(_.explore)
        val fronts = pareto(solutions)
        update(fronts)
        fronts.head
      }.toSet
    }.head

  def run(iterations: Int): MultiTspSolution =
    runPareto(iterations).head

  def probabilities(edges: Set[Edge], solution: MultiTspSolution): Map[Edge, Double] = {
    val dividers = edges.map(edge => edge -> Math.pow(tau(edge), alpha)).toMap
    val sum = dividers.values.sum
    dividers.map { case (edge, value) => edge -> value / sum }
  }

  def next(solution: MultiTspSolution): Option[MultiTspSolution] = {
    val actual = solution.nodes.head
    val neighbours = problem.edges.getFrom(actual)
    // When neighbour already in solution, that is not available then
    val available = neighbours.filterNot(edge => solution.nodes.contains(edge.node2))
    if (available.isEmpty) None else Some(solution.put(pick(available, probabilities(available, solution))))
  }

  def update(paretoFronts: List[Set[MultiTspSolution]]): Unit = {
    val paretoFrontsValues = paretoFronts.zipWithIndex.map { case (front, index) => front -> (10 - index) / 10 }
    paretoFrontsValues.foreach { case (solutions, factor) =>
      solutions.foreach { solution =>
        solution.edges.foreach(edge => tau = tau.updated(edge, tau(edge) + 1 * factor))
      }
    }
    tau = tau.map { case (edge, pheromone) => edge -> pheromone * (1 - rho) }
  }

  protected def emptySolution: MultiTspSolution = MultiTspSolution.Empty.startWith(problem.nodes.nodes.head)

  protected def pareto(solutions: Set[MultiTspSolution]): List[Set[MultiTspSolution]] = {
    val front = solutions.filter(solution => !solutions.exists(_.isDominatedBy(solution)))
    if (front.isEmpty) List(solutions)
    else front :: pareto(solutions.diff(front))
  }

  // Not used
  override def update(solutions: Set[MultiTspSolution], best: MultiTspSolution): Unit = {}
}