package aoc2023

import aoc.AOCApp
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.Stream

import scala.util.matching.Regex

object Day2 extends AOCApp {

  override def inputFileName: String = "AOC_2023_2.txt"

  private object Parsers:
    val r: Regex = " ([0-9]+) red".r
    val g: Regex = " ([0-9]+) green".r
    val b: Regex = " ([0-9]+) blue".r

  case class Bag(r: Int, g: Int, b: Int):
    def fitsIn(other: Bag): Boolean =
      (r <= other.r) && (g <= other.g) && (b <= other.b)

    def power: Long = r.toLong * g.toLong * b.toLong

  object Bag:
    def from(s: String): Bag =
      s
        .split(",")
        .foldLeft(Bag(0, 0, 0)) { case (bag, e) =>
          e match {
            case Parsers.r(n) => bag.copy(r = n.toInt)
            case Parsers.g(n) => bag.copy(g = n.toInt)
            case Parsers.b(n) => bag.copy(b = n.toInt)
          }
        }

    def maxed(a: Bag, b: Bag): Bag =
      Bag(math.max(a.r, b.r), math.max(a.g, b.g), math.max(a.b, b.b))

  private val hypotheticalBag = Bag(12, 13, 14)

  override def part1: Stream[IO, Unit] =
    Stream.eval(Ref.of[IO, Long](0)).flatMap { sum =>
      input
        .evalMap { game =>
          val idAndContents = game
            .stripPrefix("Game ")
            .split(":")
          val (id, contents) = (idAndContents(0), idAndContents(1))
          val samples = contents.split(";").map(Bag.from)
          val possible = samples.forall(_.fitsIn(hypotheticalBag))
          sum.update(_ + id.toInt).whenA(possible)
        }
        .onFinalize(sum.get.flatMap(sum => IO.println(s"Sum is $sum")))
    }

  override def part2: Stream[IO, Unit] =
    Stream.eval(Ref.of[IO, Long](0)).flatMap { sum =>
      input
        .evalMap { game =>
          val idAndContents = game
            .stripPrefix("Game ")
            .split(":")
          val (id, contents) = (idAndContents(0), idAndContents(1))
          val samples = contents.split(";").map(Bag.from)
          val smallest = samples.reduce(Bag.maxed)
          sum.update(_ + smallest.power)
        }
        .onFinalize(sum.get.flatMap(sum => IO.println(s"Sum is $sum")))
    }

}
