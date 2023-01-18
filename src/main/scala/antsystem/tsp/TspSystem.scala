package antsystem.tsp

import antsystem.AntSystem
import com.typesafe.scalalogging.LazyLogging

case class TspSystem(problem: Tsp, quantity: Int, alpha: Double, beta: Double, rho: Double)
  extends AntSystem[Tsp, TspSolution, Edge] with LazyLogging {

  private val tauZero: Double = {
    var next = problem.nodes.nodes.head
    var available = problem.nodes.nodes.tail.toSet
    var distance = 0.0
    var tour = Set.empty[Node]
    available = available.tail
    while (available.nonEmpty) {
      val neighbours = problem.edges.getFrom(next).filterNot(edge => tour.contains(edge.node2))
      if (neighbours.nonEmpty) {
        val best = neighbours.minBy(_.distance)
        distance = distance + best.distance
        next = best.node2
        available = available -- Set(next)
        tour = tour ++ Set(next)
      } else {
        available = Set.empty
      }
    }
    distance
  }

  var tau: Map[Edge, Double] = problem.items.map(_ -> tauZero).toMap

  def run(iterations: Int): TspSolution =
    (0 to iterations).map { it =>
      val solutions = ants.map(_.explore).flatMap { solution =>
        val last = solution.nodes.head
        val first = solution.nodes.last
        problem.edges.find(last, first).map(solution.put)
      }
      val best = solutions.minBy(_.distance)
//      logger.debug(s"$it (${best.distance}): ${best.nodes.map(_.id).mkString(",")}")
      update(solutions, best)
      best
    }.minBy(_.distance)

  def probabilities(edges: Set[Edge], solution: TspSolution): Map[Edge, Double] = {
    val divider = edges.map(compute).sum
    edges.map(edge => edge -> compute(edge) / divider).toMap
  }

  def next(solution: TspSolution): Option[TspSolution] = {
    val actual = solution.nodes.head
    val neighbours = problem.edges.getFrom(actual)
    // When neighbour already in solution, that is not available then
    val available = neighbours.filterNot(edge => solution.nodes.contains(edge.node2))
    if (available.isEmpty) None else Some(solution.put(pick(available, probabilities(available, solution))))
  }

  def update(solutions: Set[TspSolution], best: TspSolution): Unit = {
    solutions.foreach { solution =>
      solution.edges.foreach(edge => tau = tau.updated(edge, tau(edge) + 1 / solution.distance))
    }
    tau = tau.map { case (edge, pheromone) => edge -> pheromone * (1 - rho) }
  }

  protected def emptySolution: TspSolution = TspSolution.Empty.startWith(problem.nodes.nodes.head)

  private def compute(edge: Edge): Double =
    Math.pow(tau(edge), alpha) * Math.pow(1 / edge.distance, beta)
}
