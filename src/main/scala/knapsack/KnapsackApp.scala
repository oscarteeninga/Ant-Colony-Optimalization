package knapsack

object KnapsackApp extends App {
  val items: Set[Item] = Set(
    Item(1, 0.0000001, 1),
    Item(2, 2, 2),
    Item(3, 3, 1),
    Item(4, 3, 1),
    Item(5, 1, 3),
    Item(6, 1, 3),
    Item(7, 1, 10),
    Item(8, 1, 10),
  )
  val knapsack: Knapsack = Knapsack(4.1, items)
  val best = Knapsack.resolve(10, 100, knapsack, 1, 1, 1)
  println(s"BEST: ${best.amounts} - ${best.items.size}")
}
