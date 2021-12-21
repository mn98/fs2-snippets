package aoc

import aoc.Day11.Octopuses.EnergyMap
import cats.effect.IO
import cats.effect.kernel.{Ref, Sync}
import cats.syntax.all._
import fs2.Stream

object Day11 extends AOCApp {

  override def inputFileName: String = "AOC_11_input.txt"

  trait Octopuses[F[_]] {
    def evolve: F[Unit]

    def flashes: F[Long]

    def grid: F[String]
  }

  object Octopuses {

    case class Coordinate(r: Int, c: Int)

    case class EnergyMap(energies: Seq[Seq[Int]] = Seq.empty) {

      def coordinates: Set[Coordinate] = {
        (for {
          r <- energies.indices
          c <- energies.head.indices
        } yield {
          Coordinate(r, c)
        }).toSet
      }

      def rowLimit: Int = rows.size

      def columnLimit: Int = rows.head.size

      def rows: Seq[Seq[Int]] = energies

      def columns: Seq[Seq[Int]] = rows.head.indices.map(i => rows.map(_ (i)))

      def energyAt(coordinate: Coordinate): Int = energies(coordinate.r)(coordinate.c)

      def neighboursAt(coordinate: Coordinate): Set[Coordinate] = {
        (for {
          r <- Set(coordinate.r - 1, coordinate.r, coordinate.r + 1).filter(r => r >= 0 && r < rowLimit)
          c <- Set(coordinate.c - 1, coordinate.c, coordinate.c + 1).filter(c => c >= 0 && c < columnLimit)
        } yield {
          Coordinate(r, c)
        }) - coordinate
      }

      def withIncreasedEnergyAt(coordinates: Set[Coordinate]): EnergyMap = {
        EnergyMap(
          rows.zipWithIndex.map { case (row, r) =>
            row.zipWithIndex.map { case (energy, c) =>
              if (coordinates.contains(Coordinate(r, c)) && energy > 0) {
                EnergyMap.increaseEnergy(energy)
              } else
                energy
            }
          }
        )
      }
    }

    object EnergyMap {
      case class Point(coordinate: Coordinate, energy: Int)

      def increaseEnergy(energy: Int): Int = if (energy < 9) energy + 1 else 0
    }

    def apply[F[_]](energies: Seq[Seq[Int]])(implicit F: Sync[F]): F[Octopuses[F]] = {
      for {
        flashCount <- Ref[F].of(0L)
        energyMap <- Ref[F].of(EnergyMap(energies))
      } yield {
        new Octopuses[F] {

          override def evolve: F[Unit] = {
            // increment all energies by 1, unconditionally
            // identify the coordinates that flashed
            // increment all neighbours of all coordinates that flashed
            // an octopus can only flash once per evolution step

            val incrementAllEnergies =
              energyMap.update { energyMap =>
                EnergyMap(energyMap.energies.map(_.map(EnergyMap.increaseEnergy)))
              }

            val findCoordinatesThatFlashed =
              energyMap.get.flatMap { energyMap =>
                F.delay(energyMap.coordinates.filter(energyMap.energyAt(_) == 0))
              }

            incrementAllEnergies >>
              findCoordinatesThatFlashed.flatMap { initiallyFlashed =>
                Ref[F].of(initiallyFlashed).flatMap { flashedDuringThisStep =>

                  val loop = Stream.unfoldLoopEval(initiallyFlashed) { flashing =>

                    val neighboursByFlashedCoordinate =
                      flashedDuringThisStep.get.flatMap { flashedDuringThisStep =>
                        energyMap.get.map { energyMap =>
                          flashing.map(energyMap.neighboursAt(_) -- (flashedDuringThisStep ++ flashing))
                        }
                      }

                    val incrementAllNeighboursPerFlashedCoordinate = neighboursByFlashedCoordinate.flatMap {
                      neighboursByFlashedCoordinate =>
                        energyMap.modify { energyMap =>
                          val newEnergyMap = neighboursByFlashedCoordinate.foldLeft(energyMap) {
                            case (energyMap, neighbours) =>
                              energyMap.withIncreasedEnergyAt(neighbours)
                          }
                          val flashingNeighbours = neighboursByFlashedCoordinate.flatten.filter { neighbour =>
                            newEnergyMap.energyAt(neighbour) == 0
                          }
                          newEnergyMap -> flashingNeighbours
                        }
                    }

                    flashedDuringThisStep.updateAndGet(_ ++ flashing).flatMap { previouslyFlashed =>
                      incrementAllNeighboursPerFlashedCoordinate.map { currentFlashed =>
                        val newFlashedCoordinates = currentFlashed -- previouslyFlashed
                        flashing.size -> {
                          if (newFlashedCoordinates.nonEmpty) Some(newFlashedCoordinates) else None
                        }
                      }
                    }
                  }

                  loop.compile.toVector.map(_.sum).map(count => println(s"Count is $count")) >>
                    flashedDuringThisStep.get.flatMap(flashed => flashCount.update(_ + flashed.size))
                }
              }
          }

          override def flashes: F[Long] = flashCount.get

          override def grid: F[String] = energyMap.get.map { energyMap =>
            s"== GRID ==\n${energyMap.energies.map(_.mkString("")).mkString("\n")}\n=========="
          }

        }
      }
    }
  }

  def read(energyMap: Ref[IO, EnergyMap]): Stream[IO, Unit] =
    input
      .takeWhile(_.nonEmpty)
      .evalMap { line =>
        energyMap.update(energyMap => EnergyMap(energyMap.energies.appended(line.map(_.toString.toInt))))
      }

  override def part1: Stream[IO, Unit] = {
    Stream.eval(Ref[IO].of(EnergyMap())).flatMap { energyMap =>
      read(energyMap) ++
        Stream.eval(energyMap.get).flatMap { energyMap =>
          Stream.eval(Octopuses[IO](energyMap.energies)).flatMap { octopuses =>
            Stream.eval(Ref[IO].of(0)).flatMap { stepCounter =>
              Stream.eval(octopuses.grid.map(grid => println(s"Initial\n$grid"))) ++
                Stream
                  .eval(octopuses.evolve >> stepCounter.update(_ + 1))
                  .repeatN(30)
                  .evalTap(_ => stepCounter.get.flatMap {
                    stepCounter => octopuses.grid.map(grid => println(s"Step $stepCounter\n$grid"))
                  })
                  .onFinalize {
                    octopuses.flashes.flatMap { flashes =>
                      IO(println(s"Counted $flashes flashes"))
                    }
                  }
            }
          }
        }
    }
  }

  override def part2: Stream[IO, Unit] = Stream.empty

}
