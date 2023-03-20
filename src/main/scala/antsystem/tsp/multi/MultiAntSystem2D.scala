package antsystem.tsp.multi

import antsystem.AntSystem2D


private[tsp] class MultiAntSystem2D(problem: MultiTsp, quantity: Int, alpha: Double, beta: Double, rho: Double, val z: Int)
  extends MultiAntSystem(problem, quantity, alpha, beta, rho) with AntSystem2D[MultiTsp, MultiTspSolution, Edge]