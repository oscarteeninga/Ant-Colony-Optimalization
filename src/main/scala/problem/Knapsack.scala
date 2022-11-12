package problem

import ant.AntColony

case class Item(id: Int, weight: Double, amount: Double)

case class Solution(capibility: Double, amounts: Double = 0, items: List[Item] = List.empty) {
  def put(item: Item): Solution = Solution(capibility - item.weight, amounts + item.amount, item :: items)
}

case class Knapsack(capibility: Double, items: Set[Item])

object Knapsack {
  def resolve(ants: Int, iterations: Int, knapsack: Knapsack, alfa: Double, beta: Double, rho: Double): Solution =
    new AntColony(knapsack, ants, alfa, beta, rho).run(iterations)
}


