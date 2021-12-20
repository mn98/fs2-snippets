package aoc

import cats.effect.IO
import cats.effect.kernel.{Ref, Sync}
import cats.syntax.all._
import fs2.Stream

object Day11 extends AOCApp {

  override def inputFileName: String = "AOC_11_input.txt"

  trait Octopuses[F[_]] {
    def evolve: F[Unit]

    def flashes: F[Long]
  }

  object Octopuses {
    def apply[F[_] : Sync](grid: Seq[Seq[Int]]): F[Octopuses[F]] = {
      for {
        flashCount <- Ref[F].of(0L)
        grid <- Ref[F].of(grid)
      } yield {
        new Octopuses[F] {
          override def evolve: F[Unit] = ???

          override def flashes: F[Long] = flashCount.get
        }
      }
    }
  }

  override def part1: Stream[IO, Unit] = ???

  override def part2: Stream[IO, Unit] = ???

}
