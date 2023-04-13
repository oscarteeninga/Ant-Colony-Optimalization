package antsystem

case class Repository[S <: Solution[_]](fronts: List[Set[S]]) {
  private val layers: Map[S, Int] =
    fronts.zipWithIndex.flatMap { case (solutions, index) => solutions.map(_ -> index) }.toMap

  def put(solutions: Set[S]): Repository[S] =
    Repository(Pareto.pareto(fronts.flatten.toSet ++ solutions))

  def which(solution: S): Int = layers(solution)
}
