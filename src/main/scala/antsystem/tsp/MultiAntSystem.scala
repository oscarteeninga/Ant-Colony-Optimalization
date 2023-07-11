package antsystem.tsp

import antsystem.AntSystem
import com.typesafe.scalalogging.LazyLogging


private[tsp] case class MultiAntSystem(problem: MultiTsp, quantity: Int, alpha: Double, beta: Double, rho: Double)
  extends AntSystem[MultiTsp, MultiTspSolution, Edge]
    with LazyLogging {

  override protected def available(solution: MultiTspSolution): Set[Edge] = {
    val actual = solution.nodes.head
    val neighbours = problem.edges.getFrom(actual)
    // When neighbour already in solution, that is not available then
    neighbours.filterNot(edge => solution.nodes.contains(edge.node2))
  }

  protected def emptySolution: MultiTspSolution = MultiTspSolution.Empty.startWith(problem.nodes.nodes.head, problem.edges.edges.head.criteriaValues.criteria.keys.toList)
}