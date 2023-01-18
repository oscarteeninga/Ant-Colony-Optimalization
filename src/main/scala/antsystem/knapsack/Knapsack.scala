package antsystem.knapsack

import antsystem.{Item, Problem, Solution}

case class Package(id: Int, weight: Double, amount: Double) extends Item

case class KnapsackSolution(items: List[Package], capacity: Double, amounts: Double = 0.0) extends Solution[Package] {
  def put(item: Package): KnapsackSolution = KnapsackSolution(item :: items, capacity - item.weight, amounts + item.amount)

  override def toString: String = items.map(_.id).mkString(",")
}

case class Knapsack(items: Set[Package], capacity: Double)
  extends Problem[KnapsackSolution, Package] {
  override type T = Map[Package, Double]
}

object Knapsack {

  def resolveNaive(knapsack: Knapsack): KnapsackSolution = {
    def resolve(items: Set[Package], acc: KnapsackSolution): KnapsackSolution = {
      val available = items.filter(_.weight <= acc.capacity)
      if (available.isEmpty) acc
      else available.map(item => resolve(items - item, acc.put(item))).maxBy(_.amounts)
    }
    resolve(knapsack.items, KnapsackSolution(List.empty, knapsack.capacity))
  }

  def resolve(ants: Int, iterations: Int, knapsack: Knapsack, alfa: Double, beta: Double, rho: Double): KnapsackSolution =
    KnapsackSystem(knapsack, ants, alfa, beta, rho).run(iterations)

  def resolve2D(ants: Int, iterations: Int, knapsack: Knapsack, alfa: Double, beta: Double, rho: Double): KnapsackSolution =
    new KnapsackSystem2D(knapsack, ants, alfa, beta, rho).run(iterations)
}