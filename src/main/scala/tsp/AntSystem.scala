package tsp

class AntSystem(problem: Tsp, numberOfAnts: Int, alpha: Double, beta: Double, rho: Double) {
  val tauZero: Double = {
    var next = problem.nodes.nodes.head
    var available = problem.nodes.nodes.tail.toSet
    var distance = 0.0
    var tour = Set.empty[Node]
    available = available.tail
    while (available.nonEmpty) {
      val neighbours = problem.edges.getFrom(next).filterNot(edge => tour.contains(edge.node2))
      if (neighbours.nonEmpty) {
        val best = neighbours.minBy(_.distance)
        distance = distance + best.distance
        next = best.node2
        available = available -- Set(next)
        tour = tour ++ Set(next)
      } else {
        available = Set.empty
      }
    }
    distance
  }

  var tau: Map[Edge, Double] = Map.empty.withDefaultValue(tauZero)

  def ants: Set[Ant] =
    (0 to numberOfAnts).map(id => Ant(id, this, Solution.Empty.startWith(problem.nodes.nodes.head))).toSet

  def run(iterations: Int): Solution =
    (0 to iterations).map { it =>
      val solutions = ants.map(_.explore).map(_.solution).flatMap { solution =>
        val last = solution.nodes.head
        val first = solution.nodes.last
        problem.edges.find(last, first).map(solution.put)
      }
      val best = solutions.minBy(_.distance)
      println(s"$it (${best.distance}): ${best.nodes.map(_.id).mkString(",")}")
      update(solutions)
      best
    }.minBy(_.distance)

  def probabilities(edges: Set[Edge]): Map[Edge, Double] = {
    val divider = edges.map(compute).sum
    edges.map(edge => edge -> compute(edge) / divider).toMap
  }

  def next(solution: Solution): Option[Edge] = {
    val actual = solution.nodes.head
    val neighbours = problem.edges.getFrom(actual)
    // When neighbour already in solution, that is not available then
    val available = neighbours.filterNot(edge => solution.nodes.contains(edge.node2))
    if (available.isEmpty) None else Some(pick(available, probabilities(available)))
  }

  def update(solutions: Set[Solution]): Unit = {
    solutions.foreach { solution =>
      solution.edges.foreach(edge => tau = tau.updated(edge, tau(edge) + 1 / solution.distance))
    }
    tau = tau.map { case (edge, pheromone) => edge -> pheromone * (1 - rho) }.withDefaultValue(tauZero)
  }

  private def pick(available: Set[Edge], p: Map[Edge, Double]): Edge = {
    val random = Math.random()
    var count = 0.0
    available.find {
      item =>
        count += p(item)
        count >= random
    }.get
  }

  private def compute(edge: Edge): Double =
    Math.pow(tau(edge), alpha) * Math.pow(1 / edge.distance, beta)
}
