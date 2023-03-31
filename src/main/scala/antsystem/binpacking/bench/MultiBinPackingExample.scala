package antsystem.binpacking.bench

import antsystem.CriteriaValues
import antsystem.binpacking.multi.Criteria.Value
import antsystem.binpacking.multi.{BinPacking, Element}

import scala.util.Random

object MultiBinPackingExample {
  val Example: BinPacking = {
    val items = (1 to 200).map(Element(_, Random.nextInt(100), CriteriaValues(Map(Value -> Random.nextInt(100)))))
    val binCapacity = 150
    BinPacking(items.toSet, binCapacity)
  }
}
