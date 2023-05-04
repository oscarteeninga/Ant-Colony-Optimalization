package antsystem

trait AntSystem2D[P <: Problem[I], S <: Solution[I], I <: Item] extends AntSystem[P, S, I] {

  private var tau2D: Map[I, Map[Int, Double]] = problem.items.map(_ -> (0 to z).map(_ -> tauZero).toMap).toMap

  private var repository: Repository[S] = Repository[S](List(Set.empty))

  def z: Int

  override def run(iterations: Int): Set[S] = {
    (0 to iterations).foreach { it =>
      val solutions = ants.map(_.explore)
      update(List(solutions))
    }
    repository.fronts.head
  }

  override protected def tauFactor(edge: I): Double = {
    tau2D(edge).values.zipWithIndex.map { case (value, index) => value * (z - index) }.sum
  }

  override protected def update(paretoFronts: List[Set[S]]): Unit = {
    val solutions = paretoFronts.flatten.toSet
    repository = repository.put(solutions)
    solutions.foreach { solution =>
      val index = Math.min(z, repository.which(solution))
      solution.items.foreach(item => tau2D = tau2D.updated(item, tau2D(item).updated(index, tau2D(item)(index) + 1)))
    }
    tau2D = tau2D.map { case (item, pheromones) => item -> pheromones.map { case (key, value) => (key, value * (1 - rho)) } }
  }
}
