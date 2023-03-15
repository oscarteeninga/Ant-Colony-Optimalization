package antsystem.multi

import com.typesafe.scalalogging.LazyLogging


class MultiAntSystem2D(problem: MultiTsp, quantity: Int, alpha: Double, beta: Double, rho: Double, z: Int)
  extends MultiAntSystem(problem, quantity, alpha, beta, rho)
    with LazyLogging {

  private var tau2D: Map[Edge, Map[Int, Double]] = problem.items.map(_ -> (0 to z).map(_ -> tauZero).toMap).toMap

  override protected def tauFactor(edge: Edge): Double = Math.max(0, tau2D(edge).values.sum)

  override protected def update(paretoFronts: List[Set[MultiTspSolution]]): Unit = {
    paretoFronts.zipWithIndex.foreach { case (solutions, index) =>
      solutions.foreach { solution =>
        val factor = (z - index) / z
        solution.edges.foreach(edge => tau2D = tau2D.updated(edge, tau2D(edge).updated(index, tau2D(edge)(index) + factor)))
      }
    }
    tau2D = tau2D.map { case (edge, pheromones) => edge -> pheromones.map { case (key, value) => (key, value * (1 - rho)) } }
  }
}