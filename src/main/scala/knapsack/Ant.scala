package knapsack

case class Ant(id: Int, colony: AntColony, solution: Solution) {

  def explore: Ant = {
    colony.next(solution) match {
      case Some(next) => Ant(id, colony, solution.put(next)).explore
      case None => this
    }
  }
}
