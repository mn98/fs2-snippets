package aoc

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all._
import fs2.Stream

import scala.collection.mutable

object Day15 extends AOCApp {

  override def inputFileName: String = "AOC_15_input.txt"

  case class Coordinate(r: Int, c: Int)

  case class Point(coordinate: Coordinate, level: Int)

  case class Cavern(riskLevels: Seq[Seq[Int]]) {

    private def rowLimit: Int = riskLevels.size

    private def columnLimit: Int = riskLevels.head.size

    def points: Seq[Point] = riskLevels.zipWithIndex.flatMap { case (row, r) =>
      row.zipWithIndex.map { case (risk, c) =>
        Point(Coordinate(r, c), risk)
      }
    }

    def start: Coordinate = Coordinate(0, 0)

    def end: Coordinate = Coordinate(rowLimit - 1, columnLimit - 1)

    def pointAt(coordinate: Coordinate): Point = points.find(_.coordinate == coordinate).get

    def graph: Map[Point, Set[Point]] = {
      points.map { point =>
        val row = point.coordinate.r
        val column = point.coordinate.c
        val neighbours = for {
          coordinate <- Seq(
            Coordinate(row - 1, column),
            Coordinate(row + 1, column),
            Coordinate(row, column - 1),
            Coordinate(row, column + 1),
          ).filter(coordinate =>
            coordinate.r >= 0 && coordinate.r < rowLimit && coordinate.c >= 0 && coordinate.c < columnLimit
          )
        } yield {
          points.find(p => p.coordinate == coordinate)
        }
        point -> neighbours.flatten.toSet
      }.toMap
    }

    def shortestDistances(from: Coordinate): Seq[(Point, Int)] = {

      val queue = mutable.PriorityQueue(pointAt(from) -> 0)((a, b) => a._2 compare b._2)

      val distance = mutable.Map.from(
        points.map { point =>
          val distance = if (point.coordinate != from) Int.MaxValue else 0
          point -> distance
        }
      )

      val previous = mutable.Map.from[Point, Option[Point]](points.map(_ -> None))

      val g = graph
      println(s"Graph has ${g.size} vertices")
      var counter = 0
      while (queue.nonEmpty) {
        counter += 1
        println(s"Queue contains ${queue.size} vertices")
        val (closest, _) = queue.dequeue()
        val neighbours = g.getOrElse(closest, Set.empty)
        neighbours.collect {
          case neighbour =>
            val alternative = distance(closest) + neighbour.level
            if (alternative < distance(neighbour)) {
              distance(neighbour) = alternative
              previous(neighbour) = Some(closest)
              if (!queue.exists(_._1 == neighbour)) queue.enqueue(neighbour -> alternative)
            }
        }
      }

      println(s"Graph traversed in $counter iterations")

      distance.toSeq
    }
  }

  def read(cavern: Ref[IO, Cavern]): Stream[IO, Unit] =
    input
      .takeWhile(_.nonEmpty)
      .evalMap {
        line =>
          cavern.update(cavern => Cavern(cavern.riskLevels.appended(line.map(_.toString.toInt))))
      }
      .onFinalize {
        cavern.get.flatMap { cavern =>
          IO(println(s"== Risk ==\n${cavern.riskLevels.map(_.mkString("")).mkString("\n")}\n=========="))
        }
      }

  override def part1: Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(Cavern(Seq.empty))).flatMap { cavern =>
      read(cavern) ++
        Stream.eval {
          cavern.get.flatMap { cavern =>
            val shortestDistances = cavern.shortestDistances(cavern.start)
            //IO(println(s"Shortest distances\n${shortestDistances.mkString("\n")}")) >>
            shortestDistances.find { case (point, _) =>
              point.coordinate == cavern.end
            }.traverse_ { case (end, distance) =>
              IO(println(s"Shortest distance to ${end.coordinate} is $distance"))
            }
          }
        }
    }

  override def part2: Stream[IO, Unit] = Stream.empty

}
