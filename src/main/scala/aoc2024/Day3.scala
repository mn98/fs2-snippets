package aoc2024

import aoc.AOCApp
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.Stream

import scala.util.matching.Regex

object Day3 extends AOCApp {
  override def inputFileName: String = "2024_3.txt"

  object Patterns:
    val mul: Regex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)".r
    val on: Regex = "do\\(\\)".r
    val off: Regex = "don't\\(\\)".r
    val all: Regex = List(mul, on, off).map(_.toString).mkString("|").r

  override def part1: Stream[IO, Unit] =
    for {
      sum <- Stream.eval(Ref[IO].of(0L))
      _ <- input
        .evalMap { memory =>
          val x = Patterns.mul.findAllMatchIn(memory).map {
            case Patterns.mul(a, b) => a.toInt * b.toInt
          }.sum
          sum.update(_ + x)
        }
        .onFinalize {
          sum.get.flatMap { sum =>
            IO.println(s"Result = $sum")
          }
        }
    } yield ()

  override def part2: Stream[IO, Unit] =
    for {
      sum <- Stream.eval(Ref[IO].of(0L))
      enabled <- Stream.eval(Ref[IO].of(true))
      _ <- input
        .evalMap { memory =>
          Patterns.all.findAllMatchIn(memory).toList.traverse {
            case Patterns.mul(a, b) => enabled.get.flatMap(enabled => sum.update(_ + a.toInt * b.toInt).whenA(enabled))
            case Patterns.on() => enabled.set(true)
            case Patterns.off() => enabled.set(false)
          }
        }
        .onFinalize {
          sum.get.flatMap { sum =>
            IO.println(s"Result = $sum")
          }
        }
    } yield ()
}
