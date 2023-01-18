package antsystem.binpacking

import antsystem.{Item, Problem, Solution}

case class Rubbish(id: Int, weight: Double) extends Item

case class Bin(items: Set[Rubbish], capacity: Double) {
  def add(item: Rubbish): Bin = Bin(items ++ Set(item), capacity - item.weight)
}

case class BinPackingSolution(bins: List[Bin], left: Set[Rubbish]) extends Solution[Rubbish] {
  def bin: Bin = bins.head

  def contains(item1: Rubbish, item2: Rubbish): Boolean =
    bins.exists(bin => bin.items.contains(item1) && bin.items.contains(item2))

  def add(bin: Bin): BinPackingSolution =
    BinPackingSolution(bin :: bins, left)

  def put(item: Rubbish): BinPackingSolution =
    BinPackingSolution(bin.add(item) :: bins.tail, left - item)

  def fits(item: Rubbish): Boolean =
    bin.capacity >= item.weight

  override def toString: String =
    bins.map(bin => s"[${bin.items.map(_.id).mkString(",")}](${bin.capacity})").mkString(" ")
}

case class BinPacking(items: Set[Rubbish], bin: Double) extends Problem[BinPackingSolution, Rubbish] {
  require(items.forall(_.weight <= bin))
  override type T = Map[(Rubbish, Rubbish), Double]
}

object BinPacking {
  def resolveNaive(binPacking: BinPacking): BinPackingSolution = {
    def resolve(bins: List[Bin], left: Set[Rubbish]): BinPackingSolution = {
      if (left.isEmpty) BinPackingSolution(bins, left)
      else {
        val available: Set[Rubbish] = left.filter(BinPackingSolution(bins, left).fits(_))
        if (available.isEmpty) resolve(Bin(Set.empty, capacity = binPacking.bin) :: bins, left)
        else available.map(item => resolve(bins.head.add(item) :: bins.tail, left - item)).minBy(_.bins.size)
      }
    }

    resolve(List(Bin(Set.empty, capacity = binPacking.bin)), binPacking.items)
  }

  def resolve(ants: Int, iterations: Int, binPacking: BinPacking, alfa: Double, beta: Double, rho: Double): BinPackingSolution =
    BinPackingSystem(binPacking, ants, alfa, beta, rho).run(iterations)

  def resolve2D(ants: Int, iterations: Int, binPacking: BinPacking, alfa: Double, beta: Double, rho: Double): BinPackingSolution =
    new BinPackingSystem2D(binPacking, ants, alfa, beta, rho).run(iterations)
}
