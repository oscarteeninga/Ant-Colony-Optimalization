package antsystem

case class Ant[P <: Problem[I], S <: Solution[I], I <: Item](id: Int, system: AntSystem[P, S, I], solution: S) {
  def explore: S = {
    system.next(solution) match {
      case Some(nextSolution) => Ant(id, system, nextSolution).explore
      case None => solution
    }
  }
}
