package binpacking

object BinPackingApp extends App {

  // antsystem.Problem definition from https://developers.google.com/optimization/bin/bin_packing#java_1
  val weights = List(48, 30, 19, 36, 36, 27, 42, 42, 36, 24, 30)
  val items: Set[Item] = weights.zipWithIndex.map { case (weight, id) => Item(id, weight) }.toSet
  val packing = BinPacking(items, 100)

  def print(bins: List[Bin]): String =
    bins.map(bin => s"[${bin.items.map(_.id).mkString(",")}](${bin.capacity})").mkString(" ")

  val best = BinPacking.resolve(10, 100, packing, 0.5, 0.5, 0.5)
  println(s"BEST: ${best.bins.size} - " + print(best.bins))

  val best2D = BinPacking.resolve2D(10, 100, packing, 0.5, 0.5, 0.5)
  println(s"BEST-2D: ${best2D.bins.size} - " + print(best2D.bins))

  //  val naive = BinPacking.resolveNaive(packing)
  //  println(s"BEST-NAIVE: ${naive.bins.size} - " + print(naive.bins))
}
