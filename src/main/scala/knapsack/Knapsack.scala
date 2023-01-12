package knapsack

case class Item(id: Int, weight: Double, amount: Double)

case class Solution(capibility: Double, amounts: Double = 0, items: List[Item] = List.empty) {
  def put(item: Item): Solution = Solution(capibility - item.weight, amounts + item.amount, item :: items)
}

case class Knapsack(capibility: Double, items: Set[Item]) {
  def resolveNaive: Solution = {
    def resolve(items: Set[Item], acc: Solution): Solution = {
      val available = items.filter(_.weight <= acc.capibility)
      if (available.isEmpty) acc
      else available.map(item => resolve(items - item, acc.put(item))).maxBy(_.amounts)
    }
    resolve(items, Solution(capibility))
  }
}

object Knapsack {

  def resolveNaive(knapsack: Knapsack): Solution = knapsack.resolveNaive
  def resolve(ants: Int, iterations: Int, knapsack: Knapsack, alfa: Double, beta: Double, rho: Double): Solution =
    new AntColony(knapsack, ants, alfa, beta, rho).run(iterations)
  def resolve2D(ants: Int, iterations: Int, knapsack: Knapsack, alfa: Double, beta: Double, rho: Double): Solution =
    new AntColony2D(knapsack, ants, alfa, beta, rho).run(iterations)
}


