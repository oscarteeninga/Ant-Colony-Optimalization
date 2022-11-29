package binpacking

object BinPackingApp extends App {
  val items: Set[Item] = Set(
    Item(1, 1),
    Item(2, 2),
    Item(8, 10),
    Item(3, 1),
    Item(4, 1),
    Item(7, 10),
    Item(5, 3),
    Item(6, 3),
    Item(11, 5),
    Item(12, 3),
    Item(13, 4),
    Item(15, 1),
  )
  val packing = BinPacking(items, 10)
  val best = BinPacking.resolve(10, 100, packing, 1, 1, 1)
  println(s"BEST: ${best.bins.size}")
}
