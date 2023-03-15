package antsystem.multi

import antsystem.multi.Criteria._
import antsystem.{Item, Problem, Solution}

case class Node(id: Int)

sealed trait Criteria {
  def factor: Int = -1
  def compare(a: Double, b: Double): Int = if (a > b) factor else if (a < b) -factor else 0
}

object Criteria {
  case object Distance extends Criteria

  case object Security extends Criteria {
    override def factor: Int = 1
  }
}

case class CriteriaValues(criteria: Map[Criteria, Double] = Map(Distance -> 0, Security -> 0)) {
  def isDominatedBy(other: CriteriaValues): Boolean =
    other.criteria.forall { case (key, value) => key.compare(value, criteria(key)) != -1 } &&
      other.criteria.exists { case (key, value) => key.compare(value, criteria(key)) == 1 }

  def ++(other: CriteriaValues): CriteriaValues =
    CriteriaValues(criteria.map { case (key, value) => key -> (other.criteria(key) + value) })

  def avg: Double = criteria.map { case (key, value) => key.factor * value }.sum
}

case class Edge(node1: Node, node2: Node, criteria: CriteriaValues) extends Item

case class Nodes(nodes: Node*) {
  def get(id: Int): Node = nodes.find(_.id == id).get
}

case class Edges(edges: Edge*) {
  def getFrom(node: Node): Set[Edge] = edges.filter(_.node1 == node).toSet

  def find(node1: Node, node2: Node): Option[Edge] = edges.find(edge => edge.node1 == node1 && edge.node2 == node2)
}

case class MultiTspSolution(nodes: List[Node], edges: List[Edge], criteriaValues: CriteriaValues) extends Solution[Edge] {
  def startWith(node: Node): MultiTspSolution = MultiTspSolution(List(node), List.empty, CriteriaValues())

  def put(edge: Edge): MultiTspSolution = MultiTspSolution(edge.node2 :: nodes, edge :: edges, criteriaValues ++ edge.criteria)

  def isDominatedBy(other: MultiTspSolution): Boolean =
    criteriaValues.isDominatedBy(other.criteriaValues)

  def score: Double = criteriaValues.criteria.values.sum

  override def toString: String = criteriaValues.criteria.values.mkString("(", ":", ")")
}

object MultiTspSolution {
  val Empty: MultiTspSolution = MultiTspSolution(List.empty, List.empty, CriteriaValues())
}

case class MultiTsp(nodes: Nodes, edges: Edges) extends Problem[MultiTspSolution, Edge] {
  def neighbours(node: Node): Set[Node] = edges.getFrom(node).map(_.node2)

  override type T = Map[Edge, Double]

  override def items: Set[Edge] = edges.edges.toSet
}

object MultiTsp {
  def resolve(ants: Int, iterations: Int, problem: MultiTsp, alfa: Double, beta: Double, rho: Double): Set[MultiTspSolution] =
    MultiAntSystem(problem, ants, alfa, beta, rho).run(iterations)

  def resolve2D(ants: Int, iterations: Int, problem: MultiTsp, alfa: Double, beta: Double, rho: Double, z: Int): Set[MultiTspSolution] =
    new MultiAntSystem2D(problem, ants, alfa, beta, rho, z).run(iterations)
}

