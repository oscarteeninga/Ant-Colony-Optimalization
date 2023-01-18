package antsystem.tsp

class TspSystem2D(problem: Tsp, numberOfAnts: Int, alpha: Double, beta: Double, rho: Double)
  extends TspSystem(problem, numberOfAnts, alpha, beta, rho) {

  var tau2D: Map[Edge, Map[Edge, Double]] = problem.edges.edges.map(_ -> problem.edges.edges.map(_ -> 1.0).toMap).toMap

  override def probabilities(edges: Set[Edge], solution: TspSolution): Map[Edge, Double] = {
    val taus = edges.map(edge => edge -> Math.pow(tau2D(edge)(edge), alpha) * Math.pow(1 / edge.distance, beta)).toMap
    val divider = taus.values.sum
    taus.keys.map(edge => edge -> taus(edge) / divider).toMap
  }

  override def update(solutions: Set[TspSolution], best: TspSolution): Unit = {
    tau2D = tau2D.map { case (edge, pheromones) =>
      edge -> pheromones.map {
        case (e, pheromone) =>
          e -> (pheromone * (1 - rho) + solutions.find(_.edges.contains(e)).map(s => 1 / s.distance).sum)
      }
    }
  }
}
