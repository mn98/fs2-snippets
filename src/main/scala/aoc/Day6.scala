package aoc

import cats.effect.kernel.Ref
import cats.effect.{IO, IOApp}
import cats.syntax.all._
import fs2.io.file.{Files, Flags, Path}
import fs2.{Stream, text}

import java.nio.file.Paths

object Day6 extends IOApp.Simple {

  val input: Stream[IO, String] =
    Files[IO]
      .readAll(Path.fromNioPath(Paths.get(s"${System.getenv("HOME")}/Documents/AOC_6_input.txt")), 1024, Flags.Read)
      .through(text.utf8.decode andThen text.lines)
      .filterNot(_.isEmpty)

  val fish: Stream[IO, List[String]] = input.take(1).map(_.split(",").toList)

  def program(days: Int): Stream[IO, Unit] = {

    Stream.eval(Ref[IO].of(Seq.fill(9)(0L))).flatMap { fishByTime =>
      fish.flatMap { fish =>

        val initialize =
          Stream.emits(fish)
            .evalMap { fish =>
              fishByTime.update { fishByTime =>
                val i = fish.toInt
                fishByTime.updated(i, fishByTime(i) + 1)
              }
            }

        val evolve =
          Stream.eval {
            fishByTime.update { fishByTime =>
              fishByTime
                .drop(1)
                .appended(fishByTime.head)
                .updated(6, fishByTime(7) + fishByTime.head)
            }
          }
            .repeatN(days)
            .onFinalize {
              fishByTime.get.flatMap { fishByTime =>
                IO(println(s"There are ${fishByTime.sum} fish after $days days."))
              }
            }

        initialize ++ evolve
      }
    }
  }

  override def run: IO[Unit] = (program(80) ++ program(256)).compile.drain

}
