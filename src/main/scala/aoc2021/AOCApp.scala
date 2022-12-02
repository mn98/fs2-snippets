package aoc2021

import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Flags, Path}
import fs2.{Stream, text}

import java.nio.file.Paths

trait AOCApp extends IOApp.Simple {

  def inputFileName: String

  def input: Stream[IO, String] =
    Files[IO]
      .readAll(
        Path.fromNioPath(Paths.get(s"${System.getenv("HOME")}/Documents/$inputFileName")),
        1024,
        Flags.Read
      )
      .through(text.utf8.decode andThen text.lines)

  def part1: Stream[IO, Unit]

  def part2: Stream[IO, Unit]

  override def run: IO[Unit] = (part1 ++ part2).compile.drain

}
