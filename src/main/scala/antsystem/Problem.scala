package antsystem

trait Problem[S <: Solution[I], I <: Item] {
  type Item = I
  type Solution = S
  type T

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
