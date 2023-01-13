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

  implicit class SetOps[A](set: Set[A]) {
    def join(another: Set[A]): Set[(A, A)] =
      for {
        x <- set
        y <- another
      } yield (x, y)
  }
  def resolveNaive(binPacking: BinPacking): Solution = {
    def resolve(bins: List[Bin], left: Set[Item]): Solution = {
      if (left.isEmpty) Solution(bins, left)
      else {
        val available: Set[Item] = left.filter(Solution(bins, left).fits)
        if (available.isEmpty) resolve(Bin(Set.empty, capacity = binPacking.bin) :: bins, left)
        else available.map(item => resolve(bins.head.add(item) :: bins.tail, left - item)).minBy(_.bins.size)
      }
    }
    resolve(List(Bin(Set.empty, capacity = binPacking.bin)), binPacking.items)
  }
  def resolve(ants: Int, iterations: Int, binPacking: BinPacking, alfa: Double, beta: Double, rho: Double): Solution =
    new AntColony(binPacking, ants, alfa, beta, rho).run(iterations)

  def resolve2D(ants: Int, iterations: Int, binPacking: BinPacking, alfa: Double, beta: Double, rho: Double): Solution =
    new AntColony2D(binPacking, ants, alfa, beta, rho).run(iterations)
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




