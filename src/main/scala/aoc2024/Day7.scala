package aoc2024

import aoc.AOCApp
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.Stream

object Day7 extends AOCApp {
  override def inputFileName: String = "2024_7.txt"

  def bits(x: Long, base: Long): List[Long] = {
    var r = List.empty[Long]
    var y = x
    while (y > 0) {
      r = r.appended(y % base)
      y /= base
    }
    r
  }

  override def part1: Stream[IO, Unit] =
    for {
      sum <- Stream.eval(Ref[IO].of(0L))
      _ <- input
        .evalMap { line =>
          val valueAndTerms = line.split(":")
          val value = valueAndTerms(0).trim.toLong
          val terms = valueAndTerms(1).trim.split(" ").toList.map(_.toLong)
          val ops = 0L until math.pow(2, terms.size).toLong
          ops.find { ops =>
            val result = terms
              .tail
              .zipWithIndex
              .foldLeft(terms.head) { case (result, (term, i)) =>
                if (ops & (1L << i)) > 0 then result + term else result * term
              }
            value == result
          }.traverse_ { _ =>
            sum.update(_ + value)
          }
        }
        .onFinalize {
          sum.get.flatMap(sum => IO.println(s"Calibration result = $sum"))
        }
    } yield ()

  override def part2: Stream[IO, Unit] =
    for {
      sum <- Stream.eval(Ref[IO].of(0L))
      _ <- input
        .evalMap { line =>
          val valueAndTerms = line.split(":")
          val value = valueAndTerms(0).trim.toLong
          val terms = valueAndTerms(1).trim.split(" ").toList.map(_.toLong)
          val ops = 0L until math.pow(3, terms.size).toLong
          ops.find { ops =>
            val operators = bits(ops, 3)
            val result = terms
              .tail
              .zipWithIndex
              .foldLeft(terms.head) { case (result, (term, i)) =>
                operators.get(i) match {
                  case Some(2) => s"$result$term".toLong
                  case Some(1) => result * term
                  case _ => result + term
                }
              }
            value == result
          }.traverse_ { _ =>
            sum.update(_ + value)
          }
        }
        .onFinalize {
          sum.get.flatMap(sum => IO.println(s"Calibration result = $sum"))
        }
    } yield ()
}
