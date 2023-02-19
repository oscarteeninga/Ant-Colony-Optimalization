package antsystem.bench

import antsystem.Solution
import antsystem.binpacking.{BinPacking, BinPackingSolution}
import antsystem.knapsack.{Knapsack, KnapsackSolution}
import antsystem.multitsp.{MultiTsp, MultiTspSolution}
import antsystem.tsp.{Tsp, TspSolution}

import java.io.{BufferedWriter, FileWriter}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try


object Benchmark extends App {

  case class BenchmarkParameters(ants: Int, iterations: Int, alfa: Double, beta: Double, rho: Double)

  // Benchmark configuration
  private val formatter = DateTimeFormatter.ofPattern("YY-MM-dd HH:mm:ss")
  private val separator = ";"

  val parameters: List[BenchmarkParameters] = for {
    alfa <- List(0.5, 1.0, 1.5, 2.0, 3.0)
    beta <- List(0.5, 1.0, 1.5, 2.0, 3.0)
    rho <- List(0.1, 0.3, 0.5, 0.7, 0.9)
    ants <- List(10, 100)
    iterations <- List(100, 1000)
  } yield BenchmarkParameters(ants, iterations, alfa, beta, rho)

  def runAndSaveResults[S <: Solution[_]](runner: BenchmarkParameters => S, parameters: List[BenchmarkParameters]): List[List[String]] =
    parameters.map { params =>
      val score = Try(runner(params)).map(_.score.toString).getOrElse("-")
      List(params.ants.toString, params.iterations.toString, params.alfa.toString, params.beta.toString, params.rho.toString, score)
    }

  def run(problem: Knapsack, params: BenchmarkParameters): KnapsackSolution =
    Knapsack.resolve(params.ants, params.iterations, problem, params.alfa, params.beta, params.rho)

  def run(problem: BinPacking, params: BenchmarkParameters): BinPackingSolution =
    BinPacking.resolve(params.ants, params.iterations, problem, params.alfa, params.beta, params.rho)

  def run(problem: Tsp, params: BenchmarkParameters): TspSolution =
    Tsp.resolve(params.ants, params.iterations, problem, params.alfa, params.beta, params.rho)

  def run(problem: MultiTsp, params: BenchmarkParameters): Set[MultiTspSolution] =
    MultiTsp.resolve(params.ants, params.iterations, problem, params.alfa, params.beta, params.rho)

  def run2D(problem: MultiTsp, params: BenchmarkParameters, z: Int): Set[MultiTspSolution] =
    MultiTsp.resolve2D(params.ants, params.iterations, problem, params.alfa, params.beta, params.rho, z)

  def write(results: List[List[String]], dir: String): Unit = {
    val date = LocalDateTime.now().format(formatter)
    val filename = s"src/main/scala/antsystem/bench/result/$dir/$date.csv"
    val writer = new BufferedWriter(new FileWriter(filename))
    results.foreach(row => writer.write(row.mkString(separator) + "\n"))
    writer.close()
  }
//
//  write(runAndSaveResults(run(KnapsackExample.Example, _), parameters), "knapsack")
//  write(runAndSaveResults(run(BinPackingExample.Example, _), parameters), "binpacking")
//  write(runAndSaveResults(run(TspExample.Example, _), parameters), "tsp")
//  write(runAndSaveResults(run(MultiTspExample.EasyExample, _), parameters), "multitsp")
  println(run2D(MultiTspExample.Example, BenchmarkParameters(10, 1000, 0.5, 0.5, 0.5), 10).map(_.criteriaValues.criteria))
}


