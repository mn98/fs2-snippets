package aoc2022

import aoc.AOCApp
import cats.effect.IO
import fs2.Stream

object Day6 extends AOCApp {

  override def inputFileName: String = "AOC_2022_6.txt"

  override def part1: Stream[IO, Unit] = findMarker(4)

  override def part2: Stream[IO, Unit] = findMarker(14)

  def findMarker(markerSize: Int): Stream[IO, Unit] =
    input.flatMap { line =>
      Stream
        .emits(line.toCharArray.toSeq)
        .sliding(markerSize)
        .zipWithIndex
        .takeWhile((chars, _) => chars.toList.distinct.size < markerSize, true)
        .filter((chars, _) => chars.toList.distinct.size == markerSize)
        .evalMap { (chars, index) =>
          val markerIndex = index + markerSize
          IO.println(s"Unique chars are ${chars.toList.mkString}, they arrive after index $markerIndex")
        }
    }

}
