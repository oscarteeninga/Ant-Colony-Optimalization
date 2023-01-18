package antsystem

trait Problem[S <: Solution[I], I <: Item] {
  type Item = I
  type Solution = S
  type T

  def items: Set[I]
}

trait Solution[I <: Item] {
  def put(item: I): Solution[I]
}

trait Item
