package aoc

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all._
import fs2.Stream

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

object Day15 extends AOCApp {

  override def inputFileName: String = "AOC_15_input.txt"

  case class Coordinate(r: Int, c: Int)

  case class Point(coordinate: Coordinate, level: Int)

  case class Cavern(riskLevels: Seq[Seq[Int]]) {

    private val rowLimit: Int = riskLevels.size

    private val columnLimit: Int = riskLevels.headOption.map(_.size).getOrElse(0)

    val points: Seq[Point] = riskLevels.zipWithIndex.flatMap { case (row, r) =>
      row.zipWithIndex.map { case (risk, c) =>
        Point(Coordinate(r, c), risk)
      }
    }

    val start: Coordinate = Coordinate(0, 0)

    val end: Coordinate = Coordinate(rowLimit - 1, columnLimit - 1)

    def pointAt(coordinate: Coordinate): Point = points((coordinate.r * columnLimit) + coordinate.c)

    def neighboursAt(coordinate: Coordinate): Seq[Coordinate] = {
      val row = coordinate.r
      val column = coordinate.c
      for {
        coordinate <- Seq(
          Coordinate(row - 1, column),
          Coordinate(row + 1, column),
          Coordinate(row, column - 1),
          Coordinate(row, column + 1),
        ).filter(coordinate =>
          coordinate.r >= 0 && coordinate.r < rowLimit && coordinate.c >= 0 && coordinate.c < columnLimit
        )
      } yield {
        coordinate
      }
    }

    lazy val graph: Vector[Seq[Coordinate]] = {
      Vector(points.map(point => neighboursAt(point.coordinate)): _*)
    }

    def shortestDistances(from: Coordinate): Seq[(Point, Int)] = {

      val altQueue = ArrayBuffer.from(points.map(_.coordinate -> Int.MaxValue))
      altQueue(altQueue.indexOf(altQueue.find(_._1 == from).get)) = from -> 0

      val distances = ArrayBuffer.from(points.map(point => if (point.coordinate != from) Int.MaxValue else 0))

      val previousPoints = ArrayBuffer.fill[Option[Coordinate]](points.size)(None)

      val destination = end

      println(s"Graph has ${points.size} vertices")

      val t1 = System.nanoTime()
      var counter = 0
      breakable {
        while (altQueue.nonEmpty) {
          counter += 1
          println(s"Queue contains ${altQueue.size} vertices")
          val (closest, _) = altQueue.remove(altQueue.indexOf(altQueue.minBy(_._2)))
          if (closest == destination) break
          val closestIdx = (closest.r * columnLimit) + closest.c
          val neighbours = graph(closestIdx)
          neighbours.foreach { neighbour =>
            val neighbourIdx = (neighbour.r * columnLimit) + neighbour.c
            val alternative = distances(closestIdx) + pointAt(neighbour).level
            if (alternative < distances(neighbourIdx)) {
              distances(neighbourIdx) = alternative
              previousPoints(neighbourIdx) = Some(closest)
              altQueue(altQueue.indexOf(altQueue.find(_._1 == neighbour).get)) = neighbour -> alternative
            }
          }
        }
      }

      var crumb: Option[Point] = Some(pointAt(destination))
      val crumbs = ArrayBuffer[Option[Point]](crumb)
      while (crumb.isDefined) {
        val crumbIdx = (crumb.get.coordinate.r * columnLimit) + crumb.get.coordinate.c
        val previousCrumb = previousPoints(crumbIdx).map(pointAt)
        crumbs.append(previousCrumb)
        crumb = previousCrumb
      }
      val sum = crumbs.flatten.dropRight(1).map(_.level).sum
      println(s"${crumbs.flatten.size} crumbs sum to = $sum")

      val t2 = System.nanoTime()
      val duration = t2 - t1
      println(s"Graph traversed in $counter iterations in $duration nanos")

      points zip distances
    }

    def tiled(n: Int): Cavern = {
      def incremented(i: Int): Int = if (i < 9) i + 1 else 1

      val expanded: Seq[Seq[Int]] = riskLevels.map { row =>
        row ++ (1 until n).flatMap {
          var incrementedRow = row
          _ =>
            incrementedRow = incrementedRow.map(incremented)
            incrementedRow
        }
      }

      val lengthened: Seq[Seq[Int]] = (1 until n).flatMap {
        var incrementedGrid = expanded
        _ =>
          incrementedGrid = incrementedGrid.map(_.map(incremented))
          incrementedGrid
      }

      Cavern(expanded ++ lengthened)
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

  override def part1: Stream[IO, Unit] = //Stream.empty
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

  override def part2: Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(Cavern(Seq.empty))).flatMap { cavern =>
      read(cavern) ++
        Stream.eval {
          cavern.get.map(_.tiled(5)).flatTap(cavern =>
            IO(println(s"== Tiled(5) ==\n${cavern.riskLevels.map(_.mkString("")).mkString("\n")}\n=========="))
          ).flatMap { cavern =>
            val shortestDistances: Seq[(Point, Int)] = cavern.shortestDistances(cavern.start)
            shortestDistances.find { case (point, _) =>
              point.coordinate == cavern.end
            }.traverse_ { case (end, distance) =>
              IO(println(s"Shortest distance to ${end.coordinate} is $distance"))
            }
          }
        }
    }

}
