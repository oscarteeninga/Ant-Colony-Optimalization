package antsystem.tsp

import antsystem.{Item, Problem, Solution}

case class Node(id: Int)

case class Edge(node1: Node, node2: Node, distance: Double) extends Item

case class Nodes(nodes: Node*) {
  def get(id: Int): Node = nodes.find(_.id == id).get
}

case class Edges(edges: Edge*) {
  def getFrom(node: Node): Set[Edge] = edges.filter(_.node1 == node).toSet
  def find(node1: Node, node2: Node): Option[Edge] = edges.find(edge => edge.node1 == node1 && edge.node2 == node2)
}

case class TspSolution(nodes: List[Node], edges: List[Edge], distance: Double) extends Solution[Edge] {
  def startWith(node: Node): TspSolution = TspSolution(List(node), List.empty, 0)
  def put(edge: Edge): TspSolution = TspSolution(edge.node2 :: nodes, edge :: edges, distance + edge.distance)
}

object TspSolution {
  val Empty: TspSolution = TspSolution(List.empty, List.empty, 0.0)
}

case class Tsp(nodes: Nodes, edges: Edges) extends Problem[TspSolution, Edge] {
  def neighbours(node: Node): Set[Node] = edges.getFrom(node).map(_.node2)

  override type T = Map[Edge, Double]

  override def items: Set[Edge] = edges.edges.toSet
}

object Tsp {
  def resolve(ants: Int, iterations: Int, problem: Tsp, alfa: Double, beta: Double, rho: Double): TspSolution =
    TspSystem(problem, ants, alfa, beta, rho).run(iterations)

  def resolve2D(ants: Int, iterations: Int, problem: Tsp, alfa: Double, beta: Double, rho: Double): TspSolution =
    new TspSystem2D(problem, ants, alfa, beta, rho).run(iterations)
}

