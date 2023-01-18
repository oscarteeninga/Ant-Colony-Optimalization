package antsystem.knapsack

import antsystem.AntSystem

case class KnapsackSystem(problem: Knapsack, quantity: Int, alpha: Double, beta: Double, rho: Double)
  extends AntSystem[Knapsack, KnapsackSolution, Package] {

  protected var tau: Map[Package, Double] = problem.items.map(_ -> 1.0).toMap
  protected val items: Set[Package] = problem.items

  override protected def emptySolution: KnapsackSolution = KnapsackSolution(items.filter(_.weight == 0).toList, problem.capacity)

  def run(iterations: Int): KnapsackSolution =
    (0 to iterations).map { it =>
      val solutions = ants.map(_.explore)
      val best = solutions.maxBy(_.amounts)
//      logger.debug(s"$it (${best.amounts}/${best.capacity}): ${best.items.map(_.id).mkString(",")}")
      update(solutions, best)
      best
    }.maxBy(_.amounts)


  def update(solutions: Set[KnapsackSolution], best: KnapsackSolution): Unit =
    solutions.foreach(solution =>
      solution.items.foreach { item =>
        tau = tau.updated(item, tau(item) * rho + 1 / (1 + (best.amounts - solution.amounts) / best.amounts))
      }
    )

  def probabilities(available: Set[Package], solution: KnapsackSolution): Map[Package, Double] = {
    val probability = available.toList.map { item =>
      val u = item.amount / (item.weight / solution.capacity)
      Math.pow(tau(item), alpha) * Math.pow(u, beta)
    }
    val sum = probability.sum
    available.zip(probability).map { case (item, prob) => item -> prob / sum }.toMap
  }

  def next(solution: KnapsackSolution): Option[KnapsackSolution] = {
    val available = items.filterNot(solution.items.contains).filter(item => item.weight <= solution.capacity)
    Option.when(available.nonEmpty)(solution.put(pick(available, probabilities(available, solution))))
  }
}
