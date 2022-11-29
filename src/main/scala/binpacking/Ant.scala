package binpacking

case class Ant(id: Int, colony: AntColony, solution: Solution) {

  def explore: Ant =
    colony.next(solution) match {
      case sol: Solution if sol.left.isEmpty => this
      case sol => Ant(id, colony, sol).explore
    }
}
