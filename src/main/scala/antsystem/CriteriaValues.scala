package antsystem

trait Criteria {
  def factor: Int = -1

  def compare(a: Double, b: Double): Int = if (a > b) factor else if (a < b) -factor else 0
}

case class CriteriaValues(criteria: Map[Criteria, Double]) {
  def isDominatedBy(other: CriteriaValues): Boolean =
    other.criteria.forall { case (key, value) => key.compare(value, criteria(key)) != -1 } &&
      other.criteria.exists { case (key, value) => key.compare(value, criteria(key)) == 1 }

  def ++(other: CriteriaValues): CriteriaValues =
    CriteriaValues(criteria.map { case (key, value) => key -> (other.criteria(key) + value) })

  def avg: Double = criteria.map { case (key, value) => key.factor * value }.sum

  def factor(bests: Map[Criteria, Double]): Double =
    criteria.keys.map { c => criteria(c) / bests(c) }.product
}
