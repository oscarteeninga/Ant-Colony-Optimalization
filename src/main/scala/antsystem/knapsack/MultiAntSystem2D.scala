package antsystem.knapsack

import antsystem.AntSystem2D


class MultiAntSystem2D(problem: Knapsack, quantity: Int, alpha: Double, beta: Double, rho: Double, val z: Int)
  extends MultiAntSystem(problem, quantity, alpha, beta, rho) with AntSystem2D[Knapsack, KnapsackSolution, Element]