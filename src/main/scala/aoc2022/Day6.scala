package aoc2022

import aoc.AOCApp
import cats.effect.IO
import fs2.Stream

object Day6 extends AOCApp {

  override def inputFileName: String = "AOC_2022_6.txt"

  override def part1: Stream[IO, Unit] =
    input.flatMap { line =>
      Stream
        .emits(line.toCharArray.toSeq)
        .sliding(4)
        .zipWithIndex
        .filter((chars, _) => chars.toList.distinct.size == 4)
        .evalMap { (chars, index) =>
          val markerIndex = index + 4
          IO.println(s"Unique chars are ${chars.toList.mkString}, they arrive after index $markerIndex")
        }
    }

  override def part2: Stream[IO, Unit] = Stream.empty

}
