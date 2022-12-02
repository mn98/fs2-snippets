package aoc2021

import aoc.AOCApp
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all._
import fs2.Stream

object Day9 extends AOCApp {

  override def inputFileName: String = "AOC_9_input.txt"

  case class HeightMap(heights: Seq[Seq[Int]]) {
    def rows: Seq[Seq[Int]] = heights

    def columns: Seq[Seq[Int]] = rows.head.indices.map(i => rows.map(_ (i)))

    def heightAt(coordinate: HeightMap.Coordinate): Int = heights(coordinate.x)(coordinate.y)
  }

  object HeightMap {
    case class Coordinate(x: Int, y: Int)

    case class Basin(coordinates: Set[Coordinate]) {
      def size: Int = coordinates.size
    }
  }

  def read(heightMap: Ref[IO, HeightMap]): Stream[IO, Unit] =
    input
      .takeWhile(_.nonEmpty)
      .evalMap { line =>
        heightMap.update(heightMap => HeightMap(heightMap.heights.appended(line.map(_.toString.toInt))))
      }.onFinalize {
      heightMap.update { heightMap =>
        val rowLength = heightMap.rows.size
        val padding = Seq.fill(rowLength)(9)
        val withPadding = (padding +: heightMap.rows :+ padding).map(r => 9 +: r :+ 9)
        HeightMap(withPadding)
      }
    }

  def scan(
            heightMap: Ref[IO, HeightMap],
            lows: Ref[IO, Seq[HeightMap.Coordinate]]
          ): IO[Unit] =
    heightMap.get.flatMap { heightMap =>
      val lowFinders = for {
        (rowTriplet, rowIndex) <- heightMap.rows.sliding(3).map(HeightMap(_)).toSeq.zipWithIndex
        (columnTriplet, coordinate) <-
          rowTriplet.columns.sliding(3).map(HeightMap(_)).toSeq.zipWithIndex.map {
            case (columnTriplet, columnIndex) =>
              (columnTriplet, HeightMap.Coordinate(rowIndex + 1, columnIndex + 1))
          }
      } yield {
        val center = columnTriplet.heights(1)(1)
        val centerIsLowPoint = {
          Seq(
            columnTriplet.heightAt(HeightMap.Coordinate(1, 0)),
            columnTriplet.heightAt(HeightMap.Coordinate(1, 2)),
            columnTriplet.heightAt(HeightMap.Coordinate(0, 1)),
            columnTriplet.heightAt(HeightMap.Coordinate(2, 1)),
          ).forall(_ > center)
        }
        lows.update(_.appended(coordinate)).whenA(centerIsLowPoint)
      }
      lowFinders.reduce(_ >> _)
    }

  override def part1: Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(HeightMap(Seq.empty))).flatMap { heightMap =>
      Stream.eval(Ref[IO].of(Seq.empty[HeightMap.Coordinate])).flatMap { lows =>
        read(heightMap) ++
          Stream.eval(scan(heightMap, lows)).onFinalize {
            heightMap.get.flatMap { heightMap =>
              lows.get.flatMap { lows =>
                val sum = lows.map(heightMap.heightAt).sum + lows.size
                IO(println(s"The sum of the risk levels of all the low points is $sum"))
              }
            }
          }
      }
    }

  override def part2: Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(HeightMap(Seq.empty))).flatMap { heightMap =>
      Stream.eval(Ref[IO].of(Seq.empty[HeightMap.Coordinate])).flatMap { lows =>
        read(heightMap) ++
          Stream.eval(scan(heightMap, lows)) ++
          Stream.eval(heightMap.get).flatMap { heightMap =>
            Stream.eval(lows.get).flatMap { lows =>

              def findHigherNeighbours(point: HeightMap.Coordinate): Set[HeightMap.Coordinate] = {
                val height = heightMap.heightAt(point)
                Set(
                  point.copy(x = point.x - 1),
                  point.copy(x = point.x + 1),
                  point.copy(y = point.y - 1),
                  point.copy(y = point.y + 1),
                ).filter { neighbour =>
                  val neighbourHeight = heightMap.heightAt(neighbour)
                  neighbourHeight > height && neighbourHeight < 9
                }
              }

              def discoverBasin(low: HeightMap.Coordinate): Stream[IO, HeightMap.Basin] =
                Stream.eval(Ref[IO].of(Set(low))).flatMap { basinPoints =>
                  val discovery = Stream.unfoldLoopEval(Set(low)) { perimeter =>
                    val neighbours = perimeter.flatMap(findHigherNeighbours)
                    val updateBasin = basinPoints.updateAndGet(_ ++ neighbours)
                    updateBasin.map { basinPoints =>
                      HeightMap.Basin(basinPoints) ->
                        (if (neighbours.nonEmpty) Some(neighbours) else None)
                    }
                  }
                  discovery.drain ++ Stream.eval(basinPoints.get.map(HeightMap.Basin))
                }

              Stream
                .emits(lows)
                .flatMap(discoverBasin)
                .fold(Seq.empty[HeightMap.Basin])((basins, basin) => basins :+ basin)
                .evalMap { basins =>
                  val product = basins.sortBy(_.size).reverse.take(3).map(_.size).product
                  IO(println(s"The product of the sizes of the three largest basins is $product"))
                }
            }
          }
      }
    }

}
