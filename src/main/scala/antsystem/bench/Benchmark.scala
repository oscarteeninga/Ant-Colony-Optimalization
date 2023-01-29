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
  val formatter = DateTimeFormatter.ofPattern("YY-MM-dd HH:mm:ss")
  val separator = ";"
  val range: List[Double] = (1 to 10).map(_ / 10.0).toList
  val ants = 10
  val iterations = 1000

  val parameters: List[BenchmarkParameters] = for {
    alfa <- range
    beta <- range
    rho <- range
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

  def write(results: List[List[String]], dir: String): Unit = {
    val date = LocalDateTime.now().format(formatter)
    val filename = s"src/main/scala/antsystem/bench/result/$dir/$date.csv"
    val writer = new BufferedWriter(new FileWriter(filename))
    results.foreach(row => writer.write(row.mkString(separator) + "\n"))
    writer.close()
  }

//  write(runAndSaveResults(run(KnapsackExample.Example, _), parameters), "knapsack")
//  write(runAndSaveResults(run(BinPackingExample.Example, _), parameters), "binpacking")
//  write(runAndSaveResults(run(TspExample.Example, _), parameters), "tsp")

  println(run(MultiTspExample.EasyExample, BenchmarkParameters(ants, iterations, 0.5, 0.5, 0.5)).map(_.criteriaValues.criteria))
}


