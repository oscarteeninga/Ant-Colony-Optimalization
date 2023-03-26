package antsystem

import com.typesafe.scalalogging.LazyLogging

import scala.util.{Failure, Success, Try}

trait AntSystem[P <: Problem[S, I], S <: Solution[I], I <: Item] extends LazyLogging {

  protected val tauZero: Double = 1.0

  protected val problem: P

  protected def quantity: Int

  protected def alpha: Double

  protected def beta: Double

  protected def rho: Double

  protected def emptySolution: S

  protected var tau: Map[I, Double] = problem.items.map(_ -> tauZero).toMap

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
        solution.items.foreach(edge => tau = tau.updated(edge, tau(edge) + factor))
      }
    }
    tau = tau.map { case (edge, pheromone) => edge -> pheromone * (1 - rho) }
  }

  protected def probabilities(available: Set[I]): Map[I, Double] = {
    val dividers = available.map(edge => edge -> Math.pow(tauFactor(edge), alpha) * Math.pow(heuristicFactor(edge), beta)).toMap
    val sum = dividers.values.sum
    if (sum == 0) dividers.map { case (edge, _) => edge -> 1.0 / dividers.size }
    else dividers.map { case (edge, value) => edge -> value / sum }
  }

  protected def heuristicFactor(item: I): Double = Math.max(0, item.criteriaValues.avg)

  protected def tauFactor(item: I): Double = Math.max(0, tau(item))

  protected def pick(available: Set[I], p: Map[I, Double]): I = {
    val random = Math.random()
    var count = 0.0
    available.find {
      item =>
        count += p(item)
        count >= random
    }.get
  }
}
