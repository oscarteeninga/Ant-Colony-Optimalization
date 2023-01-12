package knapsack

case class Ant(id: Int, colony: AntColony, solution: Solution) {
  def explore: Ant = {
    colony.next(solution) match {
      case Some(next) => Ant(id, colony, solution.put(next)).explore
      case None => this
    }
  }
}
class AntColony(problem: Knapsack, numberOfAnts: Int, alpha: Double, beta: Double, rho: Double) {

  private var tau: Map[Item, Double] = problem.items.map(_ -> 1.0).toMap
  protected val items: Set[Item] = problem.items

  def ants: Set[Ant] = (0 to numberOfAnts).map(id => Ant(id, this, Solution(problem.capibility))).toSet

  def run(iterations: Int): Solution =
    (0 to iterations).map { it =>
      val solutions = ants.map(_.explore).map(_.solution)
      val best = solutions.maxBy(_.amounts)
//      println(s"$it (${best.amounts}/${best.capibility}): ${best.items.mkString(",")}")
      update(solutions, best.amounts)
      best
    }.maxBy(_.amounts)


  def update(solutions: Set[Solution], best: Double): Unit =
    solutions.foreach(solution =>
      solution.items.foreach { item =>
        tau = tau.updated(item, tau(item) * rho + 1 / (1 + (best - solution.amounts) / best))
      }
    )

  def probabilities(available: Set[Item], solution: Solution): Map[Item, Double] = {
    val u = available.map(item => item -> item.amount / (item.weight / solution.capibility)).toMap
    val probability = available.toList.map(item => Math.pow(tau(item), alpha) * Math.pow(u(item), beta))
    val sum = probability.sum
    available.zip(probability).map { case (item, prob) => item -> prob / sum }.toMap
  }

  def next(solution: Solution): Option[Item] = {
    val available: Set[Item] = items.filterNot(solution.items.contains).filter(_.weight <= solution.capibility)
    pick(available, probabilities(available, solution))
  }

  protected def pick(available: Set[Item], p: Map[Item, Double]): Option[Item] = {
    val random = Math.random()
    var count = 0.0
    available.find {
      item =>
        count += p(item)
        count >= random
    }
  }
}
