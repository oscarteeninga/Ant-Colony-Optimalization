package tsp

case class Node(id: Int)

case class Edge(node1: Node, node2: Node, distance: Double)

case class Nodes(nodes: Node*) {
  def get(id: Int): Node = nodes.find(_.id == id).get
}

case class Edges(edges: Edge*) {
  def getFrom(node: Node): Set[Edge] = edges.filter(_.node1 == node).toSet
  def find(node1: Node, node2: Node): Option[Edge] = edges.find(edge => edge.node1 == node1 && edge.node2 == node2)
}

case class Tsp(nodes: Nodes, edges: Edges) {
  def neighbours(node: Node): Set[Node] = edges.getFrom(node).map(_.node2)
}

object Tsp {
  def resolve(ants: Int, iterations: Int, problem: Tsp, alfa: Double, beta: Double, rho: Double): Solution =
    new AntSystem(problem, ants, alfa, beta, rho).run(iterations)

  def resolve2D(ants: Int, iterations: Int, problem: Tsp, alfa: Double, beta: Double, rho: Double): Solution =
    new AntSystem2D(problem, ants, alfa, beta, rho).run(iterations)
}

case class Solution(nodes: List[Node], edges: List[Edge], distance: Double) {
  def startWith(node: Node): Solution = Solution(List(node), List.empty, 0)
  def put(edge: Edge): Solution = Solution(edge.node2 :: nodes, edge :: edges, distance + edge.distance)
}

object Solution {
  val Empty: Solution = Solution(List.empty, List.empty, 0.0)
}
