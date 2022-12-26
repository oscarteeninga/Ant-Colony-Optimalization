package tsp

case class Node(id: Int, x: Double, y: Double)

case class Edge(node1: Node, node2: Node) {
  val distance: Double = Math.sqrt(Math.pow(node1.x - node2.x, 2) + Math.pow(node1.y - node2.y, 2))
}

case class Nodes(nodes: Node*) {
  def get(id: Int): Node = nodes.find(_.id == id).get
}

case class Edges(edges: Edge*) {
  def getFrom(node: Node): Set[Edge] = edges.filter(_.node1 == node).toSet
  def find(node1: Node, node2: Node): Option[Edge] = edges.find(_ == Edge(node1, node2))
}

case class Tsp(nodes: Nodes, edges: Edges) {
  def neighbours(node: Node): Set[Node] = edges.getFrom(node).map(_.node2)
}

object Tsp {
  def resolve(ants: Int, iterations: Int, problem: Tsp, alfa: Double, beta: Double, rho: Double): Solution =
    new AntSystem(problem, ants, alfa, beta, rho).run(iterations)
}

case class Solution(nodes: List[Node], distance: Double) {
  def startWith(node: Node): Solution = Solution(List(node), 0)
  def put(edge: Edge): Solution = Solution(edge.node2 :: nodes, distance + edge.distance)
  def edges: List[Edge] = nodes.zip(nodes.tail ++ List(nodes.head)).map { case (node1, node2) => Edge(node1, node2) }
}

object Solution {
  val Empty: Solution = Solution(List.empty, 0.0)
}
