package antsystem.binpacking

import antsystem.CriteriaValues
import antsystem.binpacking.Criteria.Value

import scala.util.Random

object MultiBinPackingExample {
  val Example: BinPacking = {
    val items = (1 to 200).map(Element(_, Random.nextInt(100), CriteriaValues(Map(Value -> Random.nextInt(100)))))
    val binCapacity = 100
    BinPacking(items.toSet, binCapacity)
  }
}
