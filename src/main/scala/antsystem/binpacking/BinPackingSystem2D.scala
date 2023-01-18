package antsystem.binpacking

class BinPackingSystem2D(problem: BinPacking, quantity: Int, alpha: Double, beta: Double, rho: Double)
  extends BinPackingSystem(problem, quantity, alpha, beta, rho) {

  protected var tau2D: Map[(Rubbish, Rubbish), Map[(Rubbish, Rubbish), Double]] =
    tau.keys.map(_ -> tau.keys.map(_ -> 1.0).toMap).toMap

  override def update(solutions: Set[BinPackingSolution], best: BinPackingSolution): Unit = {
    tau2D = tau2D.map { case ((item1, item2), pheromoneMap) =>
      val mij = solutions.count(_.contains(item1, item2))
      val fs = best.bins.map(bin => Math.pow(bin.items.map(_.weight).sum / problem.bin, 2)).sum / best.bins.size
      (item1, item2) -> pheromoneMap.map { case ((one, two), pheromone) =>
        (one, two) ->  (pheromone * (1 - rho) + mij * fs)
      }
    }
  }

  override def getTau(bin: Bin, item: Rubbish): Double =
    if (bin.items.isEmpty) 1.0
    else bin.items.map(newItem => tau2D.flatMap(_._2.get((item, newItem))).sum).sum / bin.items.size

  override def probabilities(available: Set[Rubbish], solution: BinPackingSolution): Map[Rubbish, Double] = {
    val bin = solution.bin
    val divider = available.map(item => Math.pow(item.weight, beta) * getTau(bin, item)).sum
    available.map(item => item -> getTau(bin, item) * Math.pow(item.weight, beta) / divider).toMap
  }
}
