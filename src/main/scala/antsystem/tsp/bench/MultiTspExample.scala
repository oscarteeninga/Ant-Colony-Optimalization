package antsystem.tsp.bench

import antsystem.CriteriaValues
import antsystem.tsp.multi.Criteria.{Distance, Security}
import antsystem.tsp.multi._

object MultiTspExample {

  // https://www.researchgate.net/figure/An-instance-of-the-TSP-with-two-criteria_fig1_336396504
  val EasyExample: MultiTsp = {
    val criteria = List(
      List((0, 0), (1, 15), (2, 14), (4, 12), (12, 4)),
      List((1, 15), (0, 0), (16, 0), (13, 3), (11, 5)),
      List((2, 14), (16, 0), (0, 0), (9, 7), (15, 1)),
      List((4, 12), (13, 3), (9, 7), (0, 0), (0, 16)),
      List((12, 4), (11, 5), (15, 1), (0, 16), (0, 0)),
    )
    val nodes = criteria.indices.map(Node)
    val edges = criteria.zip(nodes).flatMap { case (cr, node1) => cr.zip(nodes).map {
      case (c, node2) => Edge(node1, node2, CriteriaValues(Map(Distance -> c._1, Security -> c._2)))
    }}
    MultiTsp(Nodes(nodes: _*), Edges(edges: _*))
  }

  val Example: MultiTsp = {

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
    val securityMatrix = Array(
      Array(0, 543, 423, 2543, 213, 3214, 1213, 2131, 4321, 1231, 653, 342, 123),
      Array(543, 0, 3424, 1321, 4325, 123, 4325, 123, 342, 2143, 2432, 1234, 9776),
      Array(5433, 422, 0, 1343, 323, 435, 4254, 123, 5343, 1432, 1213, 143, 1232),
      Array(543, 4232, 1421, 0, 5435, 1234, 563, 312, 1243, 5345, 1243, 1280, 54),
      Array(3432, 3531, 533, 1231, 0, 5431, 5341, 645, 6453, 1232, 6445, 323, 3435),
      Array(523, 1432, 6456, 8324, 532, 0, 135, 1354, 1765, 153, 3543, 22, 1254),
      Array(142, 5253, 1254, 212, 531, 164, 0, 1432, 1653, 4314, 3891, 114, 5701),
      Array(3422, 121, 5431, 5315, 633, 2643, 6542, 0, 6532, 232, 6254, 234, 532),
      Array(453, 5432, 433, 7652, 2363, 7425, 234, 6452, 0, 2632, 233, 6326, 2643),
      Array(5432, 4232, 4532, 3426, 8634, 2346, 7534, 263, 235, 0, 7624, 235, 6543),
      Array(9324, 754, 6246, 2543, 4365, 7542, 5432, 4345, 2433, 2463, 0, 5233, 654),
      Array(524, 4263, 3243, 2345, 6522, 6354, 3234, 5432, 4323, 654, 5423, 0, 5432),
      Array(343, 5432, 5432, 543, 234, 3543, 523, 543, 5432, 432, 6325, 6542, 0)
    )

    val nodes = distanceMatrix.indices.map(id => Node(id)).toList
    val edges = nodes.flatMap(node => distanceMatrix(node.id).zip(securityMatrix(node.id)).zipWithIndex.collect {
      case (criteria, nodeId) if nodeId != node.id =>
        Edge(node, Node(nodeId), CriteriaValues(Map(Distance -> criteria._1, Security -> criteria._2)))
    })

    MultiTsp(Nodes(nodes: _*), Edges(edges: _*))
  }
}
