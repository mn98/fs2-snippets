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

    def synchronized: F[Boolean]

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

      def rowLimit: Int = energies.size

      def columnLimit: Int = energies.head.size

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
          energies.zipWithIndex.map { case (row, r) =>
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

          def evolve: F[Unit] = {

            val incrementAllEnergies =
              energyMap.update { energyMap =>
                EnergyMap(energyMap.energies.map(_.map(EnergyMap.increaseEnergy)))
              }

            val findFlashingCoordinates =
              energyMap.get.flatMap { energyMap =>
                F.delay(energyMap.coordinates.filter(energyMap.energyAt(_) == 0))
              }

            incrementAllEnergies >>
              //grid.flatMap(grid => F.delay(println(s"Incremented\n$grid"))) >>
              Ref[F].of(Set.empty[Coordinate]).flatMap { flashedInThisStep =>
                findFlashingCoordinates.flatMap { flashingCoordinates =>
                  val loop = Stream.unfoldEval(flashingCoordinates) { flashingCoordinates =>

                    val propagateFlashes =
                      flashedInThisStep
                        .updateAndGet(_ ++ flashingCoordinates)
                        .flatMap { flashedInThisStep =>
                          energyMap.update { energyMap =>
                            val neighboursPerFlashingCoordinate = {
                              flashingCoordinates.map { flashingCoordinate =>
                                flashingCoordinate ->
                                  (energyMap.neighboursAt(flashingCoordinate) -- flashedInThisStep)
                              }
                            }
                            //println(s"Flashing: ${flashingCoordinates.toSeq.sortBy(coordinate => (coordinate.r, coordinate.c))}")
                            //println(s"Neighbours: ${neighboursPerFlashingCoordinate.toSeq.sortBy(coordinate => (coordinate._1.r, coordinate._1.c))}")
                            neighboursPerFlashingCoordinate.foldLeft(energyMap) {
                              case (energyMap, (_, neighbours)) => energyMap.withIncreasedEnergyAt(neighbours)
                            }
                          }
                        }

                    propagateFlashes >>
                      //grid.flatMap(grid => F.delay(println(s"Propagated\n$grid"))) >>
                      findFlashingCoordinates.flatMap { flashingCoordinates =>
                        flashedInThisStep.get.map { flashedInThisStep =>
                          val newlyFlashingCoordinates = flashingCoordinates -- flashedInThisStep
                          if (newlyFlashingCoordinates.nonEmpty) {
                            Some(() -> newlyFlashingCoordinates)
                          } else
                            None
                        }
                      }
                  }
                  loop.compile.drain
                } >>
                  flashedInThisStep.get.flatMap { flashedInThisStep =>
                    flashCount.update(_ + flashedInThisStep.size)
                  }
              }
          }

          override def flashes: F[Long] = flashCount.get

          override def synchronized: F[Boolean] = energyMap.get.map { energyMap =>
            energyMap.energies.flatten.forall(_ == 0)
          }

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
      .evalMap {
        line =>
          energyMap.update(energyMap => EnergyMap(energyMap.energies.appended(line.map(_.toString.toInt))))
      }

  override def part1: Stream[IO, Unit] = {
    Stream.eval(Ref[IO].of(EnergyMap())).flatMap {
      energyMap =>
        read(energyMap) ++
          Stream.eval(energyMap.get).flatMap { energyMap =>
            Stream.eval(Octopuses[IO](energyMap.energies)).flatMap { octopuses =>
              Stream.eval(Ref[IO].of(0)).flatMap { stepCounter =>
                Stream.eval(octopuses.grid.map(grid => println(s"Initial\n$grid"))) ++
                  Stream
                    .eval(octopuses.evolve >> stepCounter.update(_ + 1))
                    .repeatN(100)
                    .evalTap(_ => stepCounter.get.flatMap {
                      stepCounter => octopuses.grid.map(grid => println(s"Step $stepCounter\n$grid"))
                    })
                    .onFinalize {
                      octopuses.flashes.flatMap {
                        flashes =>
                          IO(println(s"Counted $flashes flashes"))
                      }
                    }
              }
            }
          }
    }
  }

  override def part2: Stream[IO, Unit] = {
    Stream.eval(Ref[IO].of(EnergyMap())).flatMap {
      energyMap =>
        read(energyMap) ++
          Stream.eval(energyMap.get).flatMap { energyMap =>
            Stream.eval(Octopuses[IO](energyMap.energies)).flatMap { octopuses =>
              Stream.eval(Ref[IO].of(0)).flatMap { stepCounter =>
                Stream.eval(octopuses.grid.map(grid => println(s"Initial\n$grid"))) ++
                  Stream
                    .unfoldLoopEval(octopuses) { octopuses =>
                      stepCounter.update(_ + 1) >>
                        octopuses.evolve >>
                        octopuses.synchronized.map { synchronized =>
                          () -> (if (synchronized) None else Some(octopuses))
                        }
                    }
                    .onFinalize {
                      stepCounter.get.flatMap { stepCounter =>
                        IO(println(s"Octopuses synchronized at step $stepCounter"))
                      }
                    }
              }
            }
          }
    }
  }

}
