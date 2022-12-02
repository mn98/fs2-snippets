package aoc2021

import aoc.AOCApp
import cats.effect.IO
import cats.effect.kernel.Ref
import fs2.Stream

object Day6 extends AOCApp {

  override def inputFileName: String = "AOC_6_input.txt"

  val fish: Stream[IO, List[String]] = input.take(1).map(_.split(",").toList)

  def evolution(days: Int): Stream[IO, Unit] = {

    Stream.eval(Ref[IO].of(Seq.fill(9)(0L))).flatMap { fishByTime =>
      fish.flatMap { fish =>

        val initialize =
          Stream.emits(fish)
            .evalMap { fish =>
              fishByTime.update { fishByTime =>
                val i = fish.toInt
                fishByTime.updated(i, fishByTime(i) + 1)
              }
            }

        val evolve =
          Stream.eval {
            fishByTime.update { fishByTime =>
              fishByTime
                .drop(1)
                .appended(fishByTime.head)
                .updated(6, fishByTime(7) + fishByTime.head)
            }
          }
            .repeatN(days)
            .onFinalize {
              fishByTime.get.flatMap { fishByTime =>
                IO(println(s"There are ${fishByTime.sum} fish after $days days."))
              }
            }

        initialize ++ evolve
      }
    }
  }

  override def part1: Stream[IO, Unit] = evolution(80)

  override def part2: Stream[IO, Unit] = evolution(256)

}
