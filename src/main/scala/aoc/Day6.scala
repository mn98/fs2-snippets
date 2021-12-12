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
      .readAll(Path.fromNioPath(Paths.get(s"${System.getenv("HOME")}/Documents/AOC_5_input.txt")), 1024, Flags.Read)
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

  val program: Stream[IO, Unit] = {
    val fish = Ref[IO].of(input.take(1).map(_.split(",").toList.map(t => Fish.at(t.toInt))))

    Stream.eval(fish).flatMap { fish =>
      Stream
        .emits(1 to 80)
        .evalMap { i =>

        }

    }

    ???
  }

  override def run: IO[Unit] = program.compile.drain

}
