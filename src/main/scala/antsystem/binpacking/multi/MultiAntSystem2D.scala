package antsystem.binpacking.multi

import antsystem.AntSystem2D


class MultiAntSystem2D(problem: BinPacking, quantity: Int, alpha: Double, beta: Double, rho: Double, val z: Int)
  extends MultiAntSystem(problem, quantity, alpha, beta, rho) with AntSystem2D[BinPacking, BinPackingSolution, Element]