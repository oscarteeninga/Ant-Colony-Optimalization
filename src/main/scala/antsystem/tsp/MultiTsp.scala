package antsystem.tsp

import antsystem.{Criteria, CriteriaValues, Item, Problem, Solution}

private[tsp] case class Node(id: Int)


private[tsp] object Criteria {
  case object Distance extends Criteria

  case object Security extends Criteria {
    override def factor: Int = 1
  }

  case object A extends Criteria

  case object B extends Criteria {
    override def factor: Int = 1
  }

  val values: List[Criteria] = List(A, B, Distance, Security)
}

private[tsp] case class Edge(node1: Node, node2: Node, criteriaValues: CriteriaValues) extends Item

private[tsp] case class Nodes(nodes: Node*) {
  def get(id: Int): Node = nodes.find(_.id == id).get
}

private[tsp] case class Edges(edges: Edge*) {
  def getFrom(node: Node): Set[Edge] = edges.filter(_.node1 == node).toSet

  def find(node1: Node, node2: Node): Option[Edge] = edges.find(edge => edge.node1 == node1 && edge.node2 == node2)
}

case class MultiTspSolution(nodes: List[Node], items: Set[Edge], criteriaValues: CriteriaValues) extends Solution[Edge] {
  def startWith(node: Node, criteria: List[Criteria]): MultiTspSolution = MultiTspSolution(List(node), Set.empty, CriteriaValues(criteria.map(_ -> 0.0).toMap))

  def put(edge: Edge): MultiTspSolution = MultiTspSolution(edge.node2 :: nodes, Set(edge) ++ items, criteriaValues ++ edge.criteriaValues)

  override def toString: String = criteriaValues.criteria.values.mkString("(", ":", ")")
}

private[tsp] object MultiTspSolution {
  val Empty: MultiTspSolution =
    MultiTspSolution(List.empty, Set.empty, CriteriaValues(Criteria.values.map(_ -> 0.0).toMap))
}

case class MultiTsp(nodes: Nodes, edges: Edges) extends Problem[Edge] {
  def neighbours(node: Node): Set[Node] = edges.getFrom(node).map(_.node2)

  override def items: Set[Edge] = edges.edges.toSet
}

object MultiTsp {
  def resolve(ants: Int, iterations: Int, problem: MultiTsp, alfa: Double, beta: Double, rho: Double): Set[MultiTspSolution] =
    MultiAntSystem(problem, ants, alfa, beta, rho).run(iterations)

  def resolve2D(ants: Int, iterations: Int, problem: MultiTsp, alfa: Double, beta: Double, rho: Double, z: Int): Set[MultiTspSolution] =
    new MultiAntSystem2D(problem, ants, alfa, beta, rho, z).run(iterations)
}

