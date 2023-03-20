package antsystem.knapsack.multi

import antsystem.{Criteria, CriteriaValues, Item, Problem, Solution}

private[knapsack] case class Element(id: Int, size: Double, criteriaValues: CriteriaValues) extends Item

private[knapsack] object Criteria {
  case object Value extends Criteria {
    override def factor: Int = 1
  }

  case object Uselessness extends Criteria

  val values: List[Criteria] = List(Value, Uselessness)
}

case class KnapsackSolution(items: Set[Element], size: Double, criteriaValues: CriteriaValues) extends Solution[Element] {
  def put(element: Element): KnapsackSolution =
    KnapsackSolution(Set(element) ++ items, size + element.size, criteriaValues ++ element.criteriaValues)

  override def toString: String = criteriaValues.criteria.values.mkString("(", ":", ")")
}

object KnapsackSolution {
  val Empty: KnapsackSolution = KnapsackSolution(Set.empty, 0, CriteriaValues(Criteria.values.map(_ -> 0.0).toMap))
}

case class Knapsack(items: Set[Element], size: Double) extends Problem[KnapsackSolution, Element] {
  override type T = Map[Element, Double]
}

object Knapsack {
  def resolve(ants: Int, iterations: Int, problem: Knapsack, alfa: Double, beta: Double, rho: Double): Set[KnapsackSolution] =
    MultiAntSystem(problem, ants, alfa, beta, rho).run(iterations)

  def resolve2D(ants: Int, iterations: Int, problem: Knapsack, alfa: Double, beta: Double, rho: Double, z: Int): Set[KnapsackSolution] =
    new MultiAntSystem2D(problem, ants, alfa, beta, rho, z).run(iterations)
}

