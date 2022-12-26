package tsp

object TspApp extends App {
  val nodes = Nodes(
    Node(1, 0, 0),
    Node(2, 1, 0),
    Node(3, 0, 1),
    Node(4, 1, 1),
  )

  val edges = Edges(
    Edge(nodes.get(1), nodes.get(2)),
    Edge(nodes.get(2), nodes.get(3)),
    Edge(nodes.get(3), nodes.get(4)),
    Edge(nodes.get(4), nodes.get(1)),
    Edge(nodes.get(2), nodes.get(4)),
    Edge(nodes.get(4), nodes.get(3)),
    Edge(nodes.get(3), nodes.get(1)),
  )

  val tsp = Tsp(nodes, edges)
  val best = Tsp.resolve(1, 100, tsp, 1, 1, 0.1)
  println(s"BEST: ${best.distance} - ${best.nodes.map(_.id).mkString(",")}")

}
