package aoc2022

import aoc.AOCApp
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all.*
import fs2.Stream

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

object Day12 extends AOCApp {

  override def inputFileName: String = "AOC_2022_12.txt"

  case class Coordinate(r: Int, c: Int)

  case class Point(coordinate: Coordinate, level: Int)

  case class Grid(levels: Seq[Seq[Int]]) {

    private val rowLimit: Int = levels.size

    private val columnLimit: Int = levels.headOption.map(_.size).getOrElse(0)

    val points: Seq[Point] = levels.zipWithIndex.flatMap { case (row, r) =>
      row.zipWithIndex.map { case (risk, c) =>
        Point(Coordinate(r, c), risk)
      }
    }

    def pointAt(coordinate: Coordinate): Point = points((coordinate.r * columnLimit) + coordinate.c)

    def neighboursAt(coordinate: Coordinate): Seq[Coordinate] = {
      val row = coordinate.r
      val column = coordinate.c
      val point = pointAt(coordinate)
      Seq(
        Coordinate(row - 1, column),
        Coordinate(row + 1, column),
        Coordinate(row, column - 1),
        Coordinate(row, column + 1),
      ).filter { coordinate =>
        coordinate.r >= 0 && coordinate.r < rowLimit &&
          coordinate.c >= 0 && coordinate.c < columnLimit
      }.filter { coordinate =>
        val level = pointAt(coordinate).level
        (level - point.level) <= 1
      }
    }

    lazy val graph: Vector[Seq[Coordinate]] = {
      Vector(points.map(point => neighboursAt(point.coordinate)): _*)
    }

    /**
     * This works but needs improving in all sorts of ways...
     */
    def shortestDistances(from: Coordinate, to: Coordinate): Seq[(Point, Int)] = {

      val queue = ArrayBuffer.from(points.map(p => p.coordinate -> (if p.coordinate != from then Int.MaxValue else 0)))

      val distances = ArrayBuffer.from(points.map(p => if (p.coordinate != from) Int.MaxValue else 0))

      breakable {
        while (queue.nonEmpty) {
          val (closest, _) = queue.remove(queue.indexOf(queue.minBy(_._2)))
          if closest == to then break
          val closestIdx = (closest.r * columnLimit) + closest.c
          val closestDistance = distances(closestIdx)
          if closestDistance == Int.MaxValue then break
          val alternative = closestDistance + 1
          val neighbours = graph(closestIdx).filter(n => queue.exists((c, _) => c == n))
          neighbours.foreach { neighbour =>
            val neighbourIdx = (neighbour.r * columnLimit) + neighbour.c
            if (alternative < distances(neighbourIdx)) {
              distances(neighbourIdx) = alternative
              queue.find(_._1 == neighbour).foreach { x =>
                queue(queue.indexOf(x)) = neighbour -> alternative
              }
            }
          }
        }
      }

      points zip distances

    }

  }

  @tailrec
  private def elevation(c: Char): Int = c match {
    case 'S' => elevation('a')
    case 'E' => elevation('z')
    case _ => c.toInt - 'a'.toInt
  }

  def read(
            grid: Ref[IO, Grid],
            startingPoint: Char => Boolean,
            starts: Ref[IO, Set[Coordinate]],
            end: Ref[IO, Coordinate],
          ): Stream[IO, Unit] =
    input
      .takeWhile(_.nonEmpty)
      .map(_.toCharArray.toList)
      .zipWithIndex
      .evalMap { case (chars, i) =>
        val row = i.toInt
        val newRow = grid.update(grid => Grid(grid.levels.appended(Seq.empty)))
        val addPoint: Int => IO[Unit] = level => grid.update { grid =>
          Grid(grid.levels.updated(row, grid.levels(row).appended(level)))
        }

        val addPoints = chars.zipWithIndex.map { case (char, j) =>
          val level = elevation(char)
          addPoint(level) >> {
            char match {
              case c if startingPoint(c) => starts.update(_ + Coordinate(row, j))
              case 'E' => end.set(Coordinate(row, j))
              case _ => IO.unit
            }
          }
        }.sequence_

        newRow >> addPoints
      }
      .onFinalize {
        grid.get.flatMap { grid =>
          IO(println(s"== Grid ==\n${grid.levels.map(_.mkString("")).mkString("\n")}\n=========="))
        }
      }

  private def hike(startingPoint: Char => Boolean): Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(Grid(Seq.empty))).flatMap { grid =>
      Stream.eval(Ref[IO].of(Set.empty[Coordinate])).flatMap { starts =>
        Stream.eval(Ref[IO].of(Coordinate(0, 0))).flatMap { end =>
          read(grid, startingPoint, starts, end) ++
            Stream.eval(
              starts.get.flatMap { starts =>
                end.get.flatMap { end =>
                  grid
                    .get
                    .map(grid => starts.map(grid.shortestDistances(_, end)))
                    .map { distances =>
                      distances.map(_.find(_._1.coordinate == end)).toSeq.sortBy(_.map(_._2))
                    }
                }
              }
            ).debug(_.mkString("\n")).drain
        }
      }
    }

  override def part1: Stream[IO, Unit] = hike(_ == 'S')

  override def part2: Stream[IO, Unit] = hike(Set('S', 'a').contains)

}
