package aoc2024

import aoc.AOCApp
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.Stream

import scala.util.matching.Regex

object Day1 extends AOCApp {
  override def inputFileName: String = "2024_1.txt"

  val pair: Regex = "([0-9]+)\\s+([0-9]+)".r

  override def part1: Stream[IO, Unit] =
    for {
      l <- Stream.eval(Ref[IO].of(List.empty[Int]))
      r <- Stream.eval(Ref[IO].of(List.empty[Int]))
      _ <- input
        .evalMap {
          case pair(left, right) =>
            l.update(_.appended(left.toInt)) >> r.update(_.appended(right.toInt))
        }
        .onFinalize {
          (l.get, r.get).mapN { (l, r) =>
            val sum = (l.sorted zip r.sorted)
              .map((l,r) => math.abs(r - l))
              .sum
            println(s"Total distance = $sum")
          }
        }
    } yield ()

  override def part2: Stream[IO, Unit] =
    for {
      l <- Stream.eval(Ref[IO].of(List.empty[Int]))
      r <- Stream.eval(Ref[IO].of(Map.empty[Int, Int]))
      _ <- input
        .evalMap {
          case pair(left, right) =>
            l.update(_.appended(left.toInt)) >>
              r.update { r =>
                val count = r.getOrElse(right.toInt, 0)
                r.updated(right.toInt, count + 1)
              }
        }
        .onFinalize {
          (l.get, r.get).mapN { (l, r) =>
            val score = l.flatMap(l => r.get(l).map(_ * l)).sum
            println(s"Similarity score = $score")
          }
        }
    } yield ()

}
