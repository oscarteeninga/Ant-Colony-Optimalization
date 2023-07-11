package antsystem.binpacking

import antsystem.binpacking.Criteria.{BinsCount, Value}
import antsystem.{Criteria, CriteriaValues, Item, Problem, Solution}

private[binpacking] case class Element(id: Int, size: Double, criteriaValues: CriteriaValues) extends Item {
  require(!criteriaValues.criteria.contains(Criteria.BinsCount))
}

private[binpacking] case class Bin(items: Set[Element], capacity: Double)

private[binpacking] object Criteria {
  case object Value extends Criteria {
    override def factor: Int = 1
  }

  case object BinsCount extends Criteria

  val values: List[Criteria] = List(Value, BinsCount)
}

case class BinPackingSolution(items: Set[Element], bins: Set[Bin], binSize: Double, criteriaValues: CriteriaValues) extends Solution[Element] {
  def put(element: Element): BinPackingSolution = {
    val newBins = bins.headOption match {
      case Some(bin) if bin.capacity >= element.size => Set(Bin(Set(element) ++ bin.items, bin.capacity - element.size)) ++ bins.tail
      case _ if binSize >= element.size => Set(Bin(Set(element), binSize - element.size)) ++ bins
      case _ => throw new Exception("Element too big, change bins capacity or remove element from set")
    }
    val valueCriteria = if (binSize < element.size) criteriaValues.criteria(Value) else criteriaValues.criteria(Value) + element.criteriaValues.criteria(Value)
    BinPackingSolution(Set(element) ++ items, newBins, binSize, CriteriaValues(Map(BinsCount -> newBins.size, Value -> valueCriteria)))
  }

  override def toString: String = criteriaValues.criteria.values.mkString("(", ":", ")")
}

object BinPackingSolution {
  def empty(binSize: Double): BinPackingSolution = BinPackingSolution(Set.empty, Set.empty, binSize, CriteriaValues(Criteria.values.map(_ -> 0.0).toMap))
}

case class BinPacking(items: Set[Element], size: Double) extends Problem[Element]

object BinPacking {
  def resolve(ants: Int, iterations: Int, problem: BinPacking, alfa: Double, beta: Double, rho: Double): Set[BinPackingSolution] =
    MultiAntSystem(problem, ants, alfa, beta, rho).run(iterations)

  def resolve2D(ants: Int, iterations: Int, problem: BinPacking, alfa: Double, beta: Double, rho: Double, z: Int): Set[BinPackingSolution] =
    new MultiAntSystem2D(problem, ants, alfa, beta, rho, z).run(iterations)
}

