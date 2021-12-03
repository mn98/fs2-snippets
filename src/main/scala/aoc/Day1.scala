package aoc

import cats.effect.Ref
import cats.effect.{IO, IOApp}
import cats.syntax.all._
import fs2.io.file.{Files, Flags, Path}
import fs2.{Stream, text}

import java.nio.file.Paths

object Day1 extends IOApp.Simple {

  val input: fs2.Stream[IO, Int] =
    Files[IO]
      .readAll(Path.fromNioPath(Paths.get(s"${System.getenv("HOME")}/Documents/AOC_1_input.txt")), 1024, Flags.Read)
      .through(text.utf8.decode andThen text.lines)
      .filterNot(_.isEmpty)
      .map(_.toInt)

  def program(size: Int): Stream[IO, Unit] = {
    Stream.eval(Ref.of[IO, Int](0)).flatMap { counter =>
      input.sliding(size + 1).evalMap { c =>
        val l = c.toList
        counter.update(_ + 1).whenA(l.last > l.head)
      }
        .onFinalize(counter.get.flatMap(n => IO(println(s"Counted $n sliding windows of size $size larger than the previous one"))))
    }
  }

  override def run: IO[Unit] = (program(1) ++ program(3)).compile.drain

}