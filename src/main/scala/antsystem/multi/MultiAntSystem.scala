package antsystem.multi

import antsystem.AntSystem
import com.typesafe.scalalogging.LazyLogging


case class MultiAntSystem(problem: MultiTsp, quantity: Int, alpha: Double, beta: Double, rho: Double)
  extends AntSystem[MultiTsp, MultiTspSolution, Edge]
    with LazyLogging {

  private val tauZero: Double = 1.0
  protected val epsilon: Double = 10e-5
  protected var tau: Map[Edge, Double] = problem.items.map(_ -> tauZero).toMap

  def runPareto(iterations: Int): Set[MultiTspSolution] =
    Pareto.pareto {
      (0 to iterations).flatMap { it =>
        val solutions = ants.map(_.explore)
        val fronts = Pareto.pareto(solutions)
        update(fronts)
        fronts.head
      }.toSet
    }.head

  def run(iterations: Int): MultiTspSolution =
    runPareto(iterations).head

  def update(paretoFronts: List[Set[MultiTspSolution]]): Unit = {
    val fronts = paretoFronts.size + 1
    paretoFronts.zipWithIndex.foreach { case (solutions, index) =>
      solutions.foreach { solution =>
        val factor = (fronts - index) / fronts
        solution.edges.foreach(edge => tau = tau.updated(edge, tau(edge) + Math.pow(factor, beta)))
      }
    }
    tau = tau.map { case (edge, pheromone) => edge -> pheromone * (1 - rho) }
  }

  def next(solution: MultiTspSolution): Option[MultiTspSolution] = {
    val actual = solution.nodes.head
    val neighbours = problem.edges.getFrom(actual)
    // When neighbour already in solution, that is not available then
    val available = neighbours.filterNot(edge => solution.nodes.contains(edge.node2))
    if (available.isEmpty) None else Some(solution.put(pick(available, probabilities(available))))
  }

  def probabilities(edges: Set[Edge]): Map[Edge, Double] = {
    val dividers = probabilitiesValues(edges)
    val sum = dividers.values.sum
    dividers.map { case (edge, value) => edge -> value / sum }
  }

  protected def probabilitiesValues(edges: Set[Edge]): Map[Edge, Double] = {
    edges.map(edge => edge -> Math.pow(Math.max(tau(edge), epsilon), alpha)).toMap
  }

  protected def emptySolution: MultiTspSolution = MultiTspSolution.Empty.startWith(problem.nodes.nodes.head)
}

object Pareto {
  def pareto(solutions: Set[MultiTspSolution]): List[Set[MultiTspSolution]] = {
    val front = solutions.filter(solution => !solutions.exists(other => solution.isDominatedBy(other)))
    val diff = solutions.diff(front)
    if (diff.isEmpty) List(front)
    else front :: pareto(diff)
  }
}