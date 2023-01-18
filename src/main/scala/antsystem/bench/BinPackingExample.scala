package antsystem.bench

import antsystem.binpacking.{BinPacking, Rubbish}

object BinPackingExample {

  val Example: BinPacking = {
    // Problem definition from https://developers.google.com/optimization/bin/bin_packing#java_1
    val weights = List(48, 30, 19, 36, 36, 27, 42, 42, 36, 24, 30, 0, 46, 44)
    val items: Set[Rubbish] = weights.zipWithIndex.map { case (weight, id) => Rubbish(id, weight) }.toSet
    BinPacking(items, 100)
  }
}
