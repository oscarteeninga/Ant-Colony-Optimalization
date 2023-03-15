package antsystem

import com.typesafe.scalalogging.LazyLogging

trait AntSystem[P <: Problem[S, I], S <: Solution[I], I <: Item] extends LazyLogging {

  protected val tauZero: Double = 1.0

  protected val problem: P
  protected def quantity: Int
  protected def alpha: Double
  protected def beta: Double
  protected def rho: Double
  protected def emptySolution: S
  protected def tau: problem.T

  def run(iterations: Int): Set[S]

  def next(solution: S): Option[S]

  protected def ants: Set[Ant[P, S, I]] = (0 to quantity).map(Ant(_, this, emptySolution)).toSet

  protected def update(paretoFronts: List[Set[S]]): Unit

  protected def probabilities(available: Set[I]): Map[I, Double]

  protected def heuristicFactor(edge: I): Double

  protected def tauFactor(edge: I): Double

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
