package aoc2023

import aoc.AOCApp
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.Stream

import scala.annotation.tailrec

object Day1 extends AOCApp {

  override def inputFileName: String = "AOC_2023_1.txt"

  private def sumCalibrationNumbers(
                                     findFirst: String => Option[Int],
                                     findLast: String => Option[Int],
                                   ): Stream[IO, Unit] =
    Stream.eval(Ref.of[IO, Long](0)).flatMap { sum =>
      input
        .evalMap { s =>
          (findFirst(s), findLast(s)).traverseN { (i, j) =>
            //IO.println(s"$s -> $i$j") *>
            sum.update(_ + s"$i$j".toLong)
          }
        }
        .unNone
        .onFinalize(sum.get.flatMap(sum => IO.println(s"Sum is $sum")))
    }

  override def part1: Stream[IO, Unit] =
    sumCalibrationNumbers(
      _.find(_.isDigit).map(_.toString.toInt),
      _.findLast(_.isDigit).map(_.toString.toInt)
    )

  private enum Digits:
    case zero, one, two, three, four, five, six, seven, eight, nine

  @tailrec
  private def findFirstDigit(s: String): Option[Int] =
    if s.isEmpty then None
    else if s.head.isDigit then Some(s.head.toString.toInt)
    else Digits.values.find(digit => s.startsWith(digit.toString)) match {
      case Some(digit) => Some(digit.ordinal)
      case None => findFirstDigit(s.drop(1))
    }

  @tailrec
  private def findLastDigit(s: String): Option[Int] =
    if s.isEmpty then None
    else if s.last.isDigit then Some(s.last.toString.toInt)
    else Digits.values.find(digit => s.endsWith(digit.toString)) match {
      case Some(digit) => Some(digit.ordinal)
      case None => findLastDigit(s.dropRight(1))
    }

  override def part2: Stream[IO, Unit] =
    sumCalibrationNumbers(findFirstDigit, findLastDigit)

}
