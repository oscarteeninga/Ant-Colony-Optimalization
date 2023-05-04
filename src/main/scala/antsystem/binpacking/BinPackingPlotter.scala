package antsystem.binpacking

import antsystem.Plotter
import antsystem.Plotter.BenchmarkParameters


object BinPackingPlotter extends Plotter[Element, BinPackingSolution] {

  override protected lazy val title: String = "Bin Packing"
  override protected lazy val xLabel: String = "Bins [lower is better]"
  override protected lazy val yLabel: String = "Value [greater is better]"
  override protected lazy val problem: BinPacking = MultiBinPackingExample.Example

  override protected def run(params: BenchmarkParameters): Set[BinPackingSolution] = {
    print(".")
    BinPacking.resolve(params.ants, params.iterations, problem, params.alfa, params.beta, params.rho)
  }

  override protected def run2D(params: BenchmarkParameters, z: Int): Set[BinPackingSolution] = {
    print(".")
    BinPacking.resolve2D(params.ants, params.iterations, problem, params.alfa, params.beta, params.rho, z)
  }
}



