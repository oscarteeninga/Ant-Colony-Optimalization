package antsystem.knapsack

import antsystem.CriteriaValues
import Criteria.{Uselessness, Value}

import scala.util.Random

object MultiKnapsackExample {
  val Example: Knapsack = {
    // Problem from https://developers.google.com/optimization/bin/knapsack#python_1
    val capacity = 500
    val values = List(360, 83, 59, 130, 431, 67, 230, 52, 93, 125, 670, 892, 600, 38, 48, 147, 78, 256, 63, 17, 120, 164, 432, 35, 92, 110, 22, 42, 50, 323, 514, 28, 87, 73, 78, 15, 26, 78, 210, 36, 85, 189, 274, 43, 33, 10, 19, 389, 276, 312)
    val uselessness = List(360, 83, 59, 130, 431, 67, 230, 52, 93, 125, 670, 892, 600, 38, 48, 147, 78, 256, 63, 17, 120, 164, 432, 35, 92, 110, 22, 42, 50, 323, 514, 28, 87, 73, 78, 15, 26, 78, 210, 36, 85, 189, 274, 43, 33, 10, 19, 389, 276, 312)
    val criterias: List[CriteriaValues] =
      values.zip(uselessness).map { case (value, uselessnes) => CriteriaValues(Map(Value -> value, Uselessness -> uselessnes * Random.nextDouble())) }
    val weights = List(7, 0, 30, 22, 80, 94, 11, 81, 70, 64, 59, 18, 0, 36, 3, 8, 15, 42, 9, 0, 42, 47, 52, 32, 26, 48, 55, 6, 29, 84, 2, 4, 18, 56, 7, 29, 93, 44, 71, 3, 86, 66, 31, 65, 0, 79, 20, 65, 52, 13)
    val items: Set[Element] = criterias.zipWithIndex.zip(weights).collect { case ((criterias, id), weight) if weight > 0 => Element(id, weight, criterias) }.take(50).toSet
    Knapsack(items, capacity)
  }
}
