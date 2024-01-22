package aoc2021

import cats.effect.kernel.Ref
import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import fs2.io.file.{Files, Flags, Path}
import fs2.{Stream, text}

object Day5 extends IOApp.Simple {

  case class Coord(x: Int, y: Int)

  case class Line(c1: Coord, c2: Coord) {
    def isHorizontal: Boolean = c1.y == c2.y

    def isVertical: Boolean = c1.x == c2.x

    def isDiagonal: Boolean = math.abs((c1.y - c2.y) / (c1.x - c2.x)) == 1
  }

  val input: Stream[IO, String] =
    Files[IO]
      .readAll(Path(getClass.getResource("/aoc/AOC_5_input.txt").getPath), 1024, Flags.Read)
      .through(text.utf8.decode andThen text.lines)
      .filterNot(_.isEmpty)

  val program: Stream[IO, Unit] = {

    val lines: Stream[IO, Line] = input.map { coords =>
      def stringToCoord(s: String): Coord = s.split(",").toList match {
        case x :: y :: Nil => Coord(x.trim.toInt, y.trim.toInt)
        case _ => throw RuntimeException(s"Bad input: $s")
      }

      coords.split("->").toList match {
        case c1 :: c2 :: Nil => Line(stringToCoord(c1), stringToCoord(c2))
        case _ => throw RuntimeException(s"Bad coordinates: $coords")
      }
    }

    Stream.eval(Ref[IO].of(Map.empty[Coord, Int])).flatMap { counter =>
      lines.debug().evalMap { line =>
          val coords: Seq[Coord] = {
            if (line.isVertical) {
              val range = if (line.c1.y < line.c2.y) line.c1.y to line.c2.y else line.c2.y to line.c1.y
              range.map(y => Coord(line.c1.x, y))
            }
            else if (line.isHorizontal) {
              val range = if (line.c1.x < line.c2.x) line.c1.x to line.c2.x else line.c2.x to line.c1.x
              range.map(x => Coord(x, line.c1.y))
            }
            else if (line.isDiagonal) {
              val xIncrement = if (line.c1.x < line.c2.x) 1 else -1
              val xRange = line.c1.x to line.c2.x by xIncrement
              val yIncrement = if (line.c1.y < line.c2.y) 1 else -1
              val yRange = line.c1.y to line.c2.y by yIncrement
              (xRange zip yRange).map { case (x, y) => Coord(x, y) }
            }
            else Nil
          }
          counter.update { counter =>
            coords.foldLeft(counter) { (counter, coord) =>
              counter.updated(coord, counter.getOrElse(coord, 0) + 1)
            }
          }
        }
        .onFinalize {
          counter.get.flatMap { counter =>
            val count = counter.count(_._2 > 1)
            IO(println(s"Number of points where at least two lines overlap is $count"))
          }
        }
    }
  }

  override def run: IO[Unit] = program.compile.drain

}
