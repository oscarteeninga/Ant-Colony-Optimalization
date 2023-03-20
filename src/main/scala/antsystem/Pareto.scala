package antsystem

object Pareto {
  def pareto[S <: Solution[_]](solutions: Set[S]): List[Set[S]] = {
    val front = solutions.filter(solution => !solutions.exists(other => solution.isDominatedBy(other)))
    val diff = solutions.diff(front)
    if (diff.isEmpty) List(front)
    else front :: pareto(diff)
  }
}
