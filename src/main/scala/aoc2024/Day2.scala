package aoc2024

import aoc.AOCApp
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.Stream

import scala.util.matching.Regex

object Day2 extends AOCApp {
  override def inputFileName: String = "2024_2.txt"

  private val levelsPattern: Regex = "([0-9]+)".r

  override def part1: Stream[IO, Unit] =
    for {
      count <- Stream.eval(Ref[IO].of(0L))
      _ <- input
        .evalMap { line =>
          val diffs = levelsPattern.findAllIn(line).toList.map(_.toInt).sliding(2).map(l => l(1) - l.head).toList
          count.update(_ + 1).whenA(
            (diffs.forall(_ > 0) || diffs.forall(_ < 0)) && diffs.forall { diff =>
              val size = math.abs(diff)
              size >= 1 && size <= 3
            }
          )
        }
        .onFinalize {
          count.get.flatMap { count =>
            IO.println(s"Safe reports = $count")
          }
        }
    } yield ()

  override def part2: Stream[IO, Unit] =
    for {
      count <- Stream.eval(Ref[IO].of(0L))
      _ <- input
        .evalMap { line =>
          val levels = levelsPattern.findAllIn(line).toList.map(_.toInt)
          def safe(levels: List[Int]): Boolean = {
            def _safe(levels: List[Int]): Boolean = {
              val diffs = levels.sliding(2).map(l => l(1) - l.head).toList
              (diffs.forall(_ > 0) || diffs.forall(_ < 0)) && diffs.forall { diff =>
                val size = math.abs(diff)
                size >= 1 && size <= 3
              }
            }
            _safe(levels) || levels.indices.map(levels.patch(_, List.empty[Int], 1)).exists(_safe)
          }

          count.update(_ + 1).whenA(safe(levels))
        }
        .onFinalize {
          count.get.flatMap { count =>
            IO.println(s"Safe reports = $count")
          }
        }
    } yield ()
}
