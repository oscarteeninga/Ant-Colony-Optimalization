package knapsack

class AntColony2D(problem: Knapsack, numberOfAnts: Int, alpha: Double, beta: Double, rho: Double)
  extends AntColony(problem, numberOfAnts, alpha, beta, rho) {

  var tau2D: Map[Item, Map[Item, Double]] = problem.items.map(_ -> problem.items.map(_ -> 1.0).toMap).toMap

  private def updatedMap(solution: Solution, best: Double): Unit = {
    def up(item: Item): Map[Item, Double] = {
      val delta = solution.items.map(it => it -> tau2D(item)(it) * (rho + 1 / (1 + (best - solution.amounts) / best))).toMap
      tau2D(item) ++ delta
    }

    solution.items.foreach { item =>
      tau2D = tau2D.updated(item, up(item))
    }
  }

  override def update(solutions: Set[Solution], best: Double): Unit =
    solutions.foreach(updatedMap(_, best))

  override def probabilities(available: Set[Item], solution: Solution): Map[Item, Double] = {
    val u = available.map(item => item -> item.amount / (item.weight / solution.capibility)).toMap
    val probability = available.toList.map(item => Math.pow(tau2D(item)(item), alpha) * Math.pow(u(item), beta))
    val sum = probability.sum
    available.zip(probability).map { case (item, prob) => item -> prob / sum }.toMap
  }
}
