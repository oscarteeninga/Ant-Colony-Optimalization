package antsystem.binpacking.bench

import antsystem.CriteriaValues
import antsystem.binpacking.multi.Criteria.Value
import antsystem.binpacking.multi.{BinPacking, Element}

import scala.util.Random

object MultiBinPackingExample {
  val Example: BinPacking = {
    val items = (1 to 100).map(Element(_, Random.nextDouble(), CriteriaValues(Map(Value -> Random.nextDouble()))))
    val binCapacity = 0.7
    BinPacking(items.toSet, binCapacity)
  }
}
