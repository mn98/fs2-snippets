package aoc

import cats.effect.{IO, IOApp, Ref}
import cats.syntax.all._
import fs2.io.file.{Files, Flags, Path}
import fs2.{Stream, text}

import java.nio.file.Paths

object Day3 extends IOApp.Simple {

  val input: fs2.Stream[IO, String] =
    Files[IO]
      .readAll(Path.fromNioPath(Paths.get(s"${System.getenv("HOME")}/Documents/AOC_3_input.txt")), 1024, Flags.Read)
      .through(text.utf8.decode andThen text.lines)
      .filterNot(_.isEmpty)

  val program: Stream[IO, Unit] = {
    Stream.eval(Ref[IO].of(0)).flatMap { counter =>
      Stream.eval(Ref[IO].of(Map.empty[Int, Int])).flatMap { bits =>
        input.evalMap { b =>
          b.reverse.zipWithIndex.map {
            case (c, i) =>
              val ci = c.toString.toInt
              bits.update(bits => bits.updated(i, bits.getOrElse(i, 0) + ci))
          }
            .toList.sequence.void >> counter.update(_ + 1)
        }
          .onFinalize(
            counter.get.flatMap { counter =>
              bits.get.flatMap { bits =>
                val gamma = bits.keys.toSeq.sorted.foldLeft("") { case (s, c) =>
                  val common = if (bits(c) * 2 > counter) 1 else 0
                  s"$common$s"
                }
                val epsilon = gamma.map(c => if (c == '0') '1' else '0')
                val gammaD = BigInt.apply(gamma, 2)
                val epsilonD = BigInt.apply(epsilon, 2)
                val power = gammaD * epsilonD
                IO(println(s"gamma is $gamma, $gammaD, epsilon is $epsilon, $epsilonD, power is $power"))
              }
            }
          )
      }
    }
  }

  override def run: IO[Unit] = program.compile.drain
}
