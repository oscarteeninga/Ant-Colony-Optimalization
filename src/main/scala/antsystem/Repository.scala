package antsystem

case class Repository[S <: Solution[_]](fronts: List[Set[S]], historical: List[Set[S]]) {
  private val layers: Map[S, Int] =
    fronts.zipWithIndex.flatMap { case (solutions, index) => solutions.map(_ -> index) }.toMap

  def put(solutions: Set[S]): Repository[S] = {
    val values = Pareto.pareto(fronts.flatten.toSet ++ solutions)
    Repository(values, historical ++ Set(values.head))
  }

  def frontsByIterations: List[(Int, Double, Double)] =
    historical.zipWithIndex.flatMap { case (fronts, idx) =>
      fronts.map { front =>
        val values = front.criteriaValues.criteria.toList
        (idx, values.head._2, values.last._2)
      }
    }

  def which(solution: S): Int = layers(solution)
}
