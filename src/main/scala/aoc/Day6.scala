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

  trait Fish {
    def timer: Int

    def age: Seq[Fish]
  }

  object Fish {
    def apply(): Fish = FishyMcFishFace(8)

    def at(t: Int): Fish = FishyMcFishFace(t)

    private case class FishyMcFishFace(timer: Int) extends Fish {
      override def age: Seq[Fish] = {
        if (timer > 0) Seq(this.copy(timer = timer - 1))
        else Seq(Fish.at(6), Fish())
      }
    }
  }

  val program1: Stream[IO, Unit] = {

    val fish = input.take(1).map(_.split(",").toList.map(t => Fish.at(t.toInt)))

    fish.flatMap { fish =>
      Stream.eval(Ref[IO].of(fish)).flatMap { fish =>
        Stream
          .eval(fish.update(_.flatMap(_.age)))
          .repeatN(80)
          .onFinalize {
            fish.get.flatMap { fish =>
              IO(println(s"There are ${fish.size} fish after 80 days."))
            }
          }
      }
    }
  }


  val program2: Stream[IO, Unit] = {

    val fish = input.take(1).map(_.split(",").toList)

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

        def evolve(days: Int) =
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

        initialize ++ evolve(256)
      }
    }
  }

  override def run: IO[Unit] = (program1 ++ program2).compile.drain

}
