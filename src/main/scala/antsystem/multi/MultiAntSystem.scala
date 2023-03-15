package antsystem.multi

import antsystem.AntSystem
import com.typesafe.scalalogging.LazyLogging


case class MultiAntSystem(problem: MultiTsp, quantity: Int, alpha: Double, beta: Double, rho: Double)
  extends AntSystem[MultiTsp, MultiTspSolution, Edge]
    with LazyLogging {

  protected var tau: Map[Edge, Double] = problem.items.map(_ -> tauZero).toMap

  def run(iterations: Int): Set[MultiTspSolution] =
    Pareto.pareto {
      (0 to iterations).flatMap { it =>
        val solutions = ants.map(_.explore)
        val fronts = Pareto.pareto(solutions)
        update(fronts)
        fronts.head
      }.toSet
    }.head

  def next(solution: MultiTspSolution): Option[MultiTspSolution] = {
    val actual = solution.nodes.head
    val neighbours = problem.edges.getFrom(actual)
    // When neighbour already in solution, that is not available then
    val available = neighbours.filterNot(edge => solution.nodes.contains(edge.node2))

    Option.when(available.nonEmpty) {
      val prob = probabilities(available)
      val picked = pick(available, prob)
      solution.put(picked)
    }
  }

  protected def update(paretoFronts: List[Set[MultiTspSolution]]): Unit = {
    val fronts = paretoFronts.size + 1
    paretoFronts.zipWithIndex.foreach { case (solutions, index) =>
      solutions.foreach { solution =>
        val factor = (fronts - index) / fronts
        solution.edges.foreach(edge => tau = tau.updated(edge, tau(edge) + factor))
      }
    }
    tau = tau.map { case (edge, pheromone) => edge -> pheromone * (1 - rho) }
  }

  protected def probabilities(edges: Set[Edge]): Map[Edge, Double] = {
    val dividers = edges.map(edge => edge -> Math.pow(tauFactor(edge), alpha) * Math.pow(heuristicFactor(edge), beta)).toMap
    val sum = dividers.values.sum
    if (sum == 0) dividers.map { case (edge, _) => edge -> 1.0 / dividers.size }
    else dividers.map { case (edge, value) => edge -> value / sum }
  }

  protected def heuristicFactor(edge: Edge): Double = Math.max(0, edge.criteria.avg)

  protected def tauFactor(edge: Edge): Double = Math.max(0, tau(edge))

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