package antsystem.binpacking

import antsystem.AntSystem

case class BinPackingSystem(problem: BinPacking, quantity: Int, alpha: Double, beta: Double, rho: Double)
  extends AntSystem[BinPacking, BinPackingSolution, Rubbish] {

  protected var tau: Map[(Rubbish, Rubbish), Double] = problem.items.flatMap(x => problem.items.map(y => (x, y) -> 1.0)).toMap

  def run(iterations: Int): BinPackingSolution =
    (0 to iterations).map { it =>
      val solutions = ants.map(_.explore)
      val best = solutions.minBy(_.bins.size)
//      logger.debug(s"$it: ${best.bins.size}/$best")
      update(solutions, best)
      best
    }.minBy(_.bins.size)


  def update(solutions: Set[BinPackingSolution], best: BinPackingSolution): Unit = {
    tau = tau.map { case ((item1, item2), pheromone) =>
      val mij = solutions.count(_.contains(item1, item2))
      val fs = best.bins.map(bin => Math.pow(bin.items.map(_.weight).sum / problem.bin, 2)).sum / best.bins.size
      (item1, item2) -> (pheromone * (1 - rho) + mij * fs)
    }.withDefaultValue(1.0)
  }

  def getTau(bin: Bin, item: Rubbish): Double =
    if (bin.items.isEmpty) 1.0
    else bin.items.map(newItem => tau((newItem, item))).sum / bin.items.size

  def probabilities(available: Set[Rubbish], solution: BinPackingSolution): Map[Rubbish, Double] = {
    val bin = solution.bin
    val divider = available.map(item => Math.pow(item.weight, beta) * getTau(bin, item)).sum
    available.map(item => item -> getTau(bin, item) * Math.pow(item.weight, beta) / divider).toMap
  }

  def next(solution: BinPackingSolution): Option[BinPackingSolution] =
    Option.when(solution.left.nonEmpty) {
      val available: Set[Rubbish] = solution.left.filter(solution.fits)
      if (available.isEmpty) solution.add(Bin(Set.empty, capacity = problem.bin))
      else solution.put(pick(available, probabilities(available, solution)))
    }

  protected def emptySolution: BinPackingSolution = {
    val (noWeighted, weighted) = problem.items.partition(_.weight == 0)
    BinPackingSolution(List(Bin(noWeighted, problem.bin)), weighted)
  }
}
