package antsystem.knapsack

class KnapsackSystem2D(problem: Knapsack, numberOfAnts: Int, alpha: Double, beta: Double, rho: Double)
  extends KnapsackSystem(problem, numberOfAnts, alpha, beta, rho) {

  var tau2D: Map[Package, Map[Package, Double]] = problem.items.map(_ -> problem.items.map(_ -> 1.0).toMap).toMap

  private def updatedMap(solution: KnapsackSolution, best: Double): Unit = {
    def up(item: Package): Map[Package, Double] = {
      val delta = solution.items.map(it => it -> tau2D(item)(it) * (rho + 1 / (1 + (best - solution.amounts) / best))).toMap
      tau2D(item) ++ delta
    }

    solution.items.foreach { item =>
      tau2D = tau2D.updated(item, up(item))
    }
  }

  override def update(solutions: Set[KnapsackSolution], best: KnapsackSolution): Unit =
    solutions.foreach(updatedMap(_, best.amounts))

  override def probabilities(available: Set[Package], solution: KnapsackSolution): Map[Package, Double] = {
    val u = available.map(item => item -> item.amount / (item.weight / solution.capacity)).toMap
    val probability = available.toList.map(item => Math.pow(tau2D.flatMap(_._2.get(item)).sum, alpha) * Math.pow(u(item), beta))
    val sum = probability.sum
    if (sum == 0) available.map(_ -> 1.0/available.size).toMap
    else available.zip(probability).map { case (item, prob) => item -> prob / sum }.toMap
  }
}
