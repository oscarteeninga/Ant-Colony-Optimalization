package antsystem.runner

import antsystem.binpacking.{BinPacking, Rubbish}
import com.typesafe.scalalogging.LazyLogging

object BinPackingApp extends App with LazyLogging {

  // Problem definition from https://developers.google.com/optimization/bin/bin_packing#java_1
  val weights = List(48, 30, 19, 36, 36, 27, 42, 42, 36, 24, 30, 0, 46, 44)
  val items: Set[Rubbish] = weights.zipWithIndex.map { case (weight, id) => Rubbish(id, weight) }.toSet
  val packing = BinPacking(items, 100)

  //  val naive = BinPacking.resolveNaive(packing)
  //  println(s"BEST-NAIVE: ${naive.bins.size} - " + print(naive.bins))

  val best = BinPacking.resolve(10, 100, packing, 0.5, 0.5, 0.5)
  logger.info(s"BEST: ${best.bins.size} - " + best)

  val best2D = BinPacking.resolve2D(10, 100, packing, 0.5, 0.5, 0.5)
  logger.info(s"BEST-2D: ${best2D.bins.size} - " + best2D)
}
