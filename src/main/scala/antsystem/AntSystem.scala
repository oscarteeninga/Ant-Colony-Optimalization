package antsystem

import com.typesafe.scalalogging.LazyLogging

trait AntSystem[P <: Problem[S, I], S <: Solution[I], I <: Item] extends LazyLogging {

  protected val problem: P
  protected def quantity: Int
  protected def alpha: Double
  protected def beta: Double
  protected def rho: Double
  protected def emptySolution: S
  protected def tau: problem.T

  def run(iterations: Int): S

  def ants: Set[Ant[P, S, I]] = (0 to quantity).map(Ant(_, this, emptySolution)).toSet

  def update(paretoFronts: List[Set[S]]): Unit

  def probabilities(available: Set[I]): Map[I, Double]

  def next(solution: S): Option[S]

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
