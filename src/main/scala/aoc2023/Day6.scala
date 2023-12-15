package aoc2023

import aoc.AOCApp
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all.*
import fs2.Stream
import syntax.*

import scala.util.matching.Regex

object Day6 extends AOCApp {
  override def inputFileName: String = "AOC_2023_6.txt"

  object Patterns:
    val times: Regex = "Time:\\s+(.+)".r
    val distances: Regex = "Distance:\\s+(.+)".r
    val number: Regex = "([0-9]+)".r

  override def part1: Stream[IO, Unit] =
    for {
      raceTimes <- Stream.eval(Ref.of[IO, Iterator[Long]](Iterator.empty))
      recordDistances <- Stream.eval(Ref.of[IO, Iterator[Long]](Iterator.empty))
      _ <- input
        .evalMap {
          case Patterns.times(times) =>
            raceTimes.set(Patterns.number.findAllIn(times).map(_.toLong)) >>
              raceTimes.get.flatMap(raceTimes => IO.println(s"race times: $raceTimes"))
          case Patterns.distances(distances) =>
            recordDistances.set(Patterns.number.findAllIn(distances).map(_.toLong)) >>
              recordDistances.get.flatMap(recordDistances => IO.println(s"record distances: $recordDistances"))
        }
        .onComplete {
          Stream.eval(Ref.of[IO, Long](1L)).flatMap { result =>
            (raceTimes.get, recordDistances.get)
              .mapN(_ zip _)
              .map(Stream.fromIterator[IO](_, 1))
              .stream
              .evalMap { (time, distance) =>
                // v = a * t1
                // d = v * t2
                // t = t1 + t2
                val f: Long => Boolean = t => (time - t) * t > distance
                val a = (1L until time).find(f)
                val b = (time - 1L to 1L by -1L).find(f)
                val wins = (a, b) match {
                  case (Some(a), Some(b)) => Some(b - a + 1)
                  case _ => None
                }
                IO.println(s"Entry: $time, $distance, a=$a, b=$b, wins = $wins") >>
                  wins.traverse_(wins => result.update(_ * wins))
              }
              .onFinalize(result.get.flatMap(result => IO.println(s"Result = $result")))
          }
        }
    } yield ()

  override def part2: Stream[IO, Unit] =
    for {
      raceTime <- Stream.eval(Ref.of[IO, Long](0L))
      recordDistance <- Stream.eval(Ref.of[IO, Long](0L))
      _ <- input
        .evalMap {
          case Patterns.times(time) =>
            raceTime.set(time.replaceAll("\\s+", "").toLong) >>
              raceTime.get.flatMap(raceTime => IO.println(s"race time: $raceTime"))
          case Patterns.distances(distance) =>
            recordDistance.set(distance.replaceAll("\\s+", "").toLong) >>
              recordDistance.get.flatMap(recordDistance => IO.println(s"record distance: $recordDistance"))
        }
        .onFinalize {
          (raceTime.get, recordDistance.get)
            .flatMapN { (time, distance) =>
              // v = a * t1
              // d = v * t2
              // t = t1 + t2
              val f: Long => Boolean = t => (time - t) * t > distance
              val a = (1L until time).find(f)
              val b = (time - 1L to 1L by -1L).find(f)
              val wins = (a, b) match {
                case (Some(a), Some(b)) => Some(b - a + 1)
                case _ => None
              }
              IO.println(s"Entry: $time, $distance, a=$a, b=$b, wins = $wins")
            }
        }
    } yield ()

}
