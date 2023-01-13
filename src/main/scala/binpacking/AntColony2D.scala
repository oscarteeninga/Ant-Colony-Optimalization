package binpacking

import binpacking.BinPacking.SetOps

class AntColony2D(problem: BinPacking, numberOfAnts: Int, alpha: Double, beta: Double, rho: Double)
  extends AntColony(problem, numberOfAnts, alpha, beta, rho) {

  private var tau2D: Map[(Item, Item), Map[(Item, Item), Double]] =
    items.join(items).map(_ -> items.join(items).map(_ -> 1.0).toMap).toMap

  override def update(solutions: Set[Solution], best: Solution): Unit = {
    tau2D = tau2D.map { case ((item1, item2), pheromoneMap) =>
      val mij = solutions.count(_.contains(item1, item2))
      val fs = best.bins.map(bin => Math.pow(bin.items.map(_.weight).sum / problem.bin, 2)).sum / best.bins.size
      (item1, item2) -> pheromoneMap.map { case ((one, two), pheromone) =>
        (one, two) ->  (pheromone * (1 - rho) + mij * fs)
      }.withDefaultValue(1.0)
    }.withDefaultValue(Map.empty.withDefaultValue(1.0))
  }

  override def getTau(bin: Bin, item: Item): Double =
    if (bin.items.isEmpty) 1.0
    else bin.items.map(newItem => tau2D.flatMap(_._2.get((item, newItem))).sum).sum / bin.items.size

  override def probabilities(available: Set[Item], bin: Bin): Map[Item, Double] = {
    val divider = available.map(item => Math.pow(item.weight, beta) * getTau(bin, item)).sum
    available.map(item => item -> getTau(bin, item) * Math.pow(item.weight, beta) / divider).toMap
  }
}
