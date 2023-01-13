package binpacking

import binpacking.BinPacking.SetOps

case class Ant(id: Int, colony: AntColony, solution: Solution) {

  def explore: Ant =
    colony.next(solution) match {
      case sol: Solution if sol.left.isEmpty => this
      case sol => Ant(id, colony, sol).explore
    }
}

class AntColony(problem: BinPacking, numberOfAnts: Int, alpha: Double, beta: Double, rho: Double) {
  val items: Set[Item] = problem.items
  var tau: Map[(Item, Item), Double] = items.join(items).map(_ -> 1.0).toMap

  def ants: Set[Ant] = (0 to numberOfAnts).map(id => Ant(id, this, Solution(List(Bin(Set.empty, problem.bin)), problem.items))).toSet

  def run(iterations: Int): Solution =
    (0 to iterations).map { it =>
      val solutions = ants.map(_.explore).map(_.solution)
      val best = solutions.minBy(_.bins.size)
      //      println(s"$it (${best.bins.size}/$best): ${best.bins.map(_.items).mkString(",")}")
      update(solutions, best)
      best
    }.minBy(_.bins.size)


  def update(solutions: Set[Solution], best: Solution): Unit = {
    tau = tau.map { case ((item1, item2), pheromone) =>
      val mij = solutions.count(_.contains(item1, item2))
      val fs = best.bins.map(bin => Math.pow(bin.items.map(_.weight).sum / problem.bin, 2)).sum / best.bins.size
      (item1, item2) -> (pheromone * (1 - rho) + mij * fs)
    }.withDefaultValue(1.0)
  }

  def getTau(bin: Bin, item: Item): Double =
    if (bin.items.isEmpty) 1.0
    else bin.items.map(newItem => tau((newItem, item))).sum / bin.items.size

  def probabilities(available: Set[Item], bin: Bin): Map[Item, Double] = {
    val divider = available.map(item => Math.pow(item.weight, beta) * getTau(bin, item)).sum
    available.map(item => item -> getTau(bin, item) * Math.pow(item.weight, beta) / divider).toMap
  }

  def next(solution: Solution): Solution = {
    val available: Set[Item] = solution.left.filter(solution.fits)
    if (available.isEmpty) solution.add(Bin(Set.empty, capacity = problem.bin))
    else solution.add(pick(available, probabilities(available, solution.bin)))
  }

  private def pick(available: Set[Item], p: Map[Item, Double]): Item = {
    val random = Math.random()
    var count = 0.0
    available.find {
      item =>
        count += p(item)
        count >= random
    }.get
  }
}
