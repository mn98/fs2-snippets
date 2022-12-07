package aoc2022

import aoc.AOCApp
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all._
import fs2.Stream

object Day3 extends AOCApp {

  override def inputFileName: String = "AOC_2022_3.txt"

  private def itemPriority(item: Char): Int = {
    if item.isUpper then item.toInt - 'A'.toInt + 27
    else item.toInt - 'a'.toInt + 1
  }

  override def part1: fs2.Stream[IO, Unit] = sumPriorities(1)

  override def part2: fs2.Stream[IO, Unit] = sumPriorities(3)

  private def sumPriorities(numberOfElves: Int): Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(0)).flatMap { prioritySum =>
      input
        .chunkN(numberOfElves)
        .map(_.toList)
        .evalMap { items =>
          val itemCollections =
            if items.size > 1 then items
            else items.head.splitAt(items.head.length / 2).toList

          itemCollections
            .map(_.toCharArray.toSet)
            .reduce(_ intersect _)
            .headOption
            .traverse_(commonItem => prioritySum.update(_ + itemPriority(commonItem)))
        }
        .onFinalize {
          prioritySum.get.flatMap { prioritySum =>
            IO.println(s"Total priority is $prioritySum")
          }
        }
    }

}
