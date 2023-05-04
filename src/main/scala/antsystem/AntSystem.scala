package antsystem

import com.typesafe.scalalogging.LazyLogging

import scala.util.Try

trait AntSystem[P <: Problem[I], S <: Solution[I], I <: Item] extends LazyLogging {

  protected val tauZero: Double = 1.0

  protected val problem: P

  protected def quantity: Int

  protected def alpha: Double

  protected def beta: Double

  protected def rho: Double

  protected def emptySolution: S

  private var tau: Map[I, Double] = problem.items.map(_ -> tauZero).toMap

  def run(iterations: Int): Set[S] = {
    Pareto.pareto {
      (0 to iterations).flatMap { it =>
        val solutions = ants.map(_.explore)
        val fronts = Pareto.pareto(solutions)
        update(fronts)
        fronts.head
      }.toSet
    }.head
  }

  def next(solution: S): Option[S] = {
    val a = available(solution)
    Option.when(a.nonEmpty) {
      val prob = probabilities(a)
      val picked = pick(a, prob)
      solution.put(picked)
    }.asInstanceOf[Option[S]]
  }

  protected def available(solution: S): Set[I]

  protected def ants: Set[Ant[P, S, I]] = (0 to quantity).map(Ant(_, this, emptySolution)).toSet

  protected def update(paretoFronts: List[Set[S]]): Unit = {
    val fronts = paretoFronts.size + 1
    paretoFronts.zipWithIndex.foreach { case (solutions, index) =>
      solutions.foreach { solution =>
        val factor = (fronts - index) / fronts
        solution.items.foreach(item => tau = tau.updated(item, tau(item) + factor))
      }
    }
    tau = tau.map { case (item, pheromone) => item -> pheromone * (1 - rho) }
  }

  private def probabilities(available: Set[I]): Map[I, Double] = {
    val dividers = available.map(item => item -> Math.pow(tauFactor(item), alpha) * Math.pow(heuristicFactor(item), beta)).toMap
    val sum = dividers.values.sum
    if (sum == 0) dividers.map { case (item, _) => item -> 1.0 / dividers.size }
    else dividers.map { case (item, value) => item -> value / sum }
  }

  protected def heuristicFactor(item: I): Double = Math.max(0, item.criteriaValues.avg)

  protected def tauFactor(item: I): Double = Math.max(0, tau(item))

  private def pick(available: Set[I], p: Map[I, Double]): I = {
    val random = Math.random()
    var count = 0.0
    Try {
      available.find {
        item =>
          count += p(item)
          count >= random
      }.get
    }.get
  }
}
