package antsystem.binpacking

import antsystem.AntSystem
import com.typesafe.scalalogging.LazyLogging

private[binpacking] case class MultiAntSystem(problem: BinPacking, quantity: Int, alpha: Double, beta: Double, rho: Double)
  extends AntSystem[BinPacking, BinPackingSolution, Element]
    with LazyLogging {

  protected def available(solution: BinPackingSolution): Set[Element] =
    problem.items.diff(solution.items)

  protected def emptySolution: BinPackingSolution = BinPackingSolution.empty(problem.size)

  override protected def heuristicFactor(item: Element): Double = Math.max(0, super.heuristicFactor(item) - item.size)
}