package aoc

import cats.effect.{IO, IOApp}
import cats.syntax.all._
import fs2.io.file.{Files, Flags, Path}
import fs2.{Stream, text}

import java.nio.file.Paths
import scala.annotation.tailrec
import scala.math.BigDecimal.RoundingMode

object Day7 extends IOApp.Simple {

  val input: Stream[IO, List[Int]] =
    Files[IO]
      .readAll(Path.fromNioPath(Paths.get(s"${System.getenv("HOME")}/Documents/AOC_7_input.txt")), 1024, Flags.Read)
      .through(text.utf8.decode andThen text.lines)
      .filterNot(_.isEmpty)
      .map(_.split(",").toList.map(_.toInt))

  def slope(f: Double => Double, x: Double, epsilon: Double): Double =
    (f(x + epsilon) - f(x - epsilon)) / 2.0 / epsilon

  def fuelRequiredForPart1(x: Double, positions: Seq[Int]): Double =
    positions.map(p => math.abs(x - p)).sum

  def fuelRequiredForPart2(x: Double, positions: Seq[Int]): Double =
    positions.map { p =>
      val n = math.abs(x - p)
      n * (n + 1) / 2.0
    }.sum

  @tailrec def bisection(f: Double => Double, a: Double, b: Double, tolerance: Double): Double = {
    val fa = f(a)
    val x = (a + b) / 2.0
    val fx = f(x)
    if (math.abs(fx) < tolerance) x
    else {
      val (aa, bb) = if (fa * fx > 0) (x, b) else (a, x)
      bisection(f, aa, bb, tolerance)
    }
  }

  def crabs(f: (Double, List[Int]) => Double): Stream[IO, Unit] =
    input.map { positions =>
      val a = positions.min.toDouble
      val b = positions.max.toDouble
      (bisection(slope(f(_, positions), _, 0.1), a, b, 0.01), positions)
    }.evalMap { case (root, positions) =>
      val rootInt = BigDecimal(root).setScale(0, RoundingMode.HALF_UP).toInt
      val fuel = BigDecimal(f(rootInt, positions)).setScale(0, RoundingMode.HALF_UP).toInt
      IO(println(s"root is $root, to nearest whole is $rootInt and fuel required is $fuel"))
    }

  val program: Stream[IO, Unit] = crabs(fuelRequiredForPart1) ++ crabs(fuelRequiredForPart2)

  override def run: IO[Unit] = program.compile.drain

}
