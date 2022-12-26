package tsp

case class Ant(id: Int, colony: AntSystem, solution: Solution) {
  def explore: Ant = {
    colony.next(solution) match {
      case Some(edge) => Ant(id, colony, solution.put(edge)).explore
      case None => this
    }
  }
}
