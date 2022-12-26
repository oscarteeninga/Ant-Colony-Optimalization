package binpacking

import scala.util.Random

case class Item(id: Int, weight: Double)

object Item {
  def random(id: Int): Item = Item(id, new Random().nextDouble())
}

case class BinPacking(items: Set[Item], bin: Double) {
  require(items.forall(_.weight <= bin))
}

object BinPacking {
  def resolve(ants: Int, iterations: Int, binPacking: BinPacking, alfa: Double, beta: Double, rho: Double): Solution =
    new AntColony(binPacking, ants, alfa, beta, rho).run(iterations)
}

case class Bin(items: Set[Item], capacity: Double) {
  def add(item: Item): Bin = Bin(items ++ Set(item), capacity - item.weight)
}

case class Solution(bins: List[Bin], left: Set[Item]) {
  def bin: Bin = bins.head

  def contains(item1: Item, item2: Item): Boolean =
    bins.exists(bin => bin.items.contains(item1) && bin.items.contains(item2))

  def add(bin: Bin): Solution =
    Solution(bin :: bins, left)

  def add(item: Item): Solution =
    Solution(bin.add(item) :: bins.tail, left - item)

  def fits(item: Item): Boolean =
    bin.capacity >= item.weight
}





