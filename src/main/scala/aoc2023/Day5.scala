package aoc2023

import aoc.AOCApp
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.Stream

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day5 extends AOCApp {

  override def inputFileName: String = "AOC_2023_5.txt"

  object Patterns:
    val seeds: Regex = "seeds: (.+)".r
    val mappings: Regex = "([a-z]+)-to-([a-z]+) map:".r
    val mapping: Regex = "([0-9]+) ([0-9]+) ([0-9]+)".r
    val blankLine: Regex = "".r
    val number: Regex = "([0-9]+)".r
    val numberPair: Regex = "([0-9]+) ([0-9]+)".r

  case class Mapping(destination: Long, source: Long, length: Long):
    def mapFrom(i: Long): Option[Long] =
      if i >= source && i < source + length then Some(destination + i - source) else None

  case class Mapper(source: String, destination: String, mappings: List[Mapping]):
    def mapFrom(i: Long): Long = {
      @tailrec
      def search(in: List[Mapping]): Long = in match {
        case Nil => i
        case head :: tail => head.mapFrom(i) match {
          case Some(j) => j
          case None => search(tail)
        }
      }

      search(mappings)
    }

  def findLocation(seed: Long, mappers: List[Mapper]): Long = {
    @tailrec
    def mapFrom(source: String, i: Long): Long = {
      mappers.find(_.source == source) match {
        case None => i
        case Some(mapper) =>
          mapFrom(mapper.destination, mapper.mapFrom(i))
      }
    }

    mapFrom("seed", seed)
  }

  override def part1: fs2.Stream[IO, Unit] =
    Stream.eval(Ref.of[IO, List[Mapper]](Nil)).flatMap { mappers =>
      Stream.eval(Ref.of[IO, List[Long]](Nil)).flatMap { seeds =>
        input
          .evalMap {
            case Patterns.blankLine() => IO.unit
            case Patterns.seeds(seedString) =>
              IO.println(s"Found seeds: $seedString") >>
                seeds.set(Patterns.number.findAllIn(seedString).map(_.toLong).toList)
            case Patterns.mappings(source, destination) =>
              IO.println(s"Found mapping: $source, $destination") >>
                mappers.update(mappers => Mapper(source, destination, Nil) :: mappers)
            case Patterns.mapping(destination, source, length) =>
              IO.println(s"Found mapping: $destination, $source, $length") >>
                mappers.update {
                  case head :: tail =>
                    val mapping = Mapping(destination.toLong, source.toLong, length.toLong)
                    val mapper = head.copy(mappings = head.mappings :+ mapping)
                    mapper :: tail
                  case Nil => Nil
                }
          }
          .onFinalize {
            (seeds.get, mappers.get).flatMapN { (seeds, mappers) =>
              val locations = seeds.map(seed => seed -> findLocation(seed, mappers))
              val lowestLocation = locations.map(_._2).min
              IO.println(s"Seeds: $seeds") >>
                IO.println(s"Mappings:\n${mappers.mkString("\n")}") >>
                IO.println(s"Locations:\n${locations.mkString("\n")}") >>
                IO.println(s"Lowest location: $lowestLocation")
            }
          }
      }
    }

  case class SeedRange(start: Long, length: Long):
    val end: Long = start + length

  override def part2: fs2.Stream[IO, Unit] =
    Stream.eval(Ref.of[IO, List[Mapper]](Nil)).flatMap { mappers =>
      Stream.eval(Ref.of[IO, List[SeedRange]](Nil)).flatMap { seeds =>
        input
          .evalMap {
            case Patterns.blankLine() => IO.unit
            case Patterns.seeds(seedString) =>
              IO.println(s"Found seeds: $seedString") >>
                seeds.update(
                  _ ++ Patterns.numberPair.findAllIn(seedString).toList.map {
                    case Patterns.numberPair(start, length) => SeedRange(start.toLong, length.toLong)
                  }
                )
            case Patterns.mappings(source, destination) =>
              IO.println(s"Found mapping: $source, $destination") >>
                mappers.update(mappers => Mapper(source, destination, Nil) :: mappers)
            case Patterns.mapping(destination, source, length) =>
              IO.println(s"Found mapping: $destination, $source, $length") >>
                mappers.update {
                  case head :: tail =>
                    val mapping = Mapping(destination.toLong, source.toLong, length.toLong)
                    val mapper = head.copy(mappings = head.mappings :+ mapping)
                    mapper :: tail
                  case Nil => Nil
                }
          }
          .onComplete {
            Stream.eval(seeds.get).flatMap { seeds =>
              Stream.eval(mappers.get).flatMap { mappers =>
                Stream.eval(Ref.of[IO, Long](Long.MaxValue)).flatMap { lowestLocation =>
                  //val locations = seeds.map(seed => seed -> findLocation(seed, mappers))
                  seeds
                    .map(seeds => Stream.emits(seeds.start until seeds.end))
                    .reduce(_ ++ _)
                    .evalMap { seed =>
                      val location = findLocation(seed, mappers)
                      lowestLocation.update(math.min(_, location))
                    }
                    .onFinalize {
                      lowestLocation.get.flatMap { lowestLocation =>
                        IO.println(s"Lowest location: $lowestLocation")
                      }
                    }
                }
              }
            }
          }
      }
    }

}
