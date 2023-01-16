package tsp

object TspApp extends App {

  val distanceMatrix = Array(
    Array(0, 2451, 713, 1018, 1631, 1374, 2408, 213, 2571, 875, 1420, 2145, 1972),
    Array(2451, 0, 1745, 1524, 831, 1240, 959, 2596, 403, 1589, 1374, 357, 579),
    Array(713, 1745, 0, 355, 920, 803, 1737, 851, 1858, 262, 940, 1453, 1260),
    Array(1018, 1524, 355, 0, 700, 862, 1395, 1123, 1584, 466, 1056, 1280, 987),
    Array(1631, 831, 920, 700, 0, 663, 1021, 1769, 949, 796, 879, 586, 371),
    Array(1374, 1240, 803, 862, 663, 0, 1681, 1551, 1765, 547, 225, 887, 999),
    Array(2408, 959, 1737, 1395, 1021, 1681, 0, 2493, 678, 1724, 1891, 1114, 701),
    Array(213, 2596, 851, 1123, 1769, 1551, 2493, 0, 2699, 1038, 1605, 2300, 2099),
    Array(2571, 403, 1858, 1584, 949, 1765, 678, 2699, 0, 1744, 1645, 653, 600),
    Array(875, 1589, 262, 466, 796, 547, 1724, 1038, 1744, 0, 679, 1272, 1162),
    Array(1420, 1374, 940, 1056, 879, 225, 1891, 1605, 1645, 679, 0, 1017, 1200),
    Array(2145, 357, 1453, 1280, 586, 887, 1114, 2300, 653, 1272, 1017, 0, 504),
    Array(1972, 579, 1260, 987, 371, 999, 701, 2099, 600, 1162, 1200, 504, 0)
  )

  val nodes = distanceMatrix.indices.map(id => Node(id)).toList

  val edges = nodes.flatMap(node => distanceMatrix(node.id).zipWithIndex.collect { case (distance, nodeId) if nodeId != node.id => Edge(node, Node(nodeId), distance) })

  val tsp = Tsp(Nodes(nodes: _*), Edges(edges: _*))
  val best = Tsp.resolve(10, 100, tsp, 1, 1, 0.5)
  println(s"BEST: ${best.distance} - ${best.nodes.map(_.id).mkString(",")}")

  val best2D = Tsp.resolve2D(10, 100, tsp, 1, 1, 0.5)
  println(s"BEST2D: ${best2D.distance} - ${best2D.nodes.map(_.id).mkString(",")}")
}
