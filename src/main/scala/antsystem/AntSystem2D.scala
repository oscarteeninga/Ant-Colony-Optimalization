package antsystem

trait AntSystem2D[P <: Problem[S, I], S <: Solution[I], I <: Item] extends AntSystem[P, S, I] {

  def z: Int

  private var tau2D: Map[I, Map[Int, Double]] = problem.items.map(_ -> (0 to z).map(_ -> tauZero).toMap).toMap

  override protected def tauFactor(edge: I): Double = Math.max(0, tau2D(edge).values.sum)

  override protected def update(paretoFronts: List[Set[S]]): Unit = {
    val half = paretoFronts.size
    val best = paretoFronts.map(_.map(_.criteriaValues.avg).max).max
    paretoFronts.zipWithIndex.take(z).foreach { case (solutions, index) =>
      solutions.foreach { solution =>
        // val factor = (z - index)
        // val factor = (half - index) / half half
        val factor = (half - index) * solution.criteriaValues.avg / half / best // half-best
        solution.items.foreach(edge => tau2D = tau2D.updated(edge, tau2D(edge).updated(index, tau2D(edge)(index) + factor)))
      }
    }
    tau2D = tau2D.map { case (edge, pheromones) => edge -> pheromones.map { case (key, value) => (key, value * (1 - rho)) } }
  }
}
