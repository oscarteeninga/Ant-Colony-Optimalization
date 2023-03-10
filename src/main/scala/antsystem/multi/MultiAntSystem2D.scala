package antsystem.multi

import com.typesafe.scalalogging.LazyLogging


class MultiAntSystem2D(problem: MultiTsp, quantity: Int, alpha: Double, beta: Double, rho: Double, z: Int)
  extends MultiAntSystem(problem, quantity, alpha, beta, rho)
    with LazyLogging {

  private val tauZero: Double = 1.0
  private var tau2D: Map[Edge, Map[Int, Double]] = problem.items.map(_ -> (0 to z).map(_ -> tauZero).toMap).toMap

  override def probabilitiesValues(edges: Set[Edge]): Map[Edge, Double] = {
    def edgeValue(edge: Edge, half: Int = z / 2): Double =
      tau2D(edge).map { case (index, value) => value * (index - half) }.sum

    edges.map(edge => edge -> Math.pow(Math.max(edgeValue(edge), epsilon), alpha)).toMap
  }

  override def update(paretoFronts: List[Set[MultiTspSolution]]): Unit = {
    paretoFronts.take(z).zipWithIndex.foreach { case (solutions, index) =>
      solutions.foreach { solution =>
        solution.edges.foreach(edge => tau2D = tau2D.updated(edge, tau2D(edge).updated(index, tau2D(edge)(index) + 1)))
      }
    }
    tau2D = tau2D.map { case (edge, pheromones) => edge -> pheromones.map { case (key, value) => (key, value * (1 - rho)) } }
  }
}