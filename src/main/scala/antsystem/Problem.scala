package antsystem

trait Problem[I <: Item] {
  def items: Set[I]
}

trait Solution[I <: Item] {
  def items: Set[I]
  def put(item: I): Solution[I]
  def criteriaValues: CriteriaValues
  def isDominatedBy(other: Solution[_]): Boolean = criteriaValues.isDominatedBy(other.criteriaValues)
}

trait Item {
  def criteriaValues: CriteriaValues
}
