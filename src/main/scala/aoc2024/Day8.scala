package aoc2024

import aoc.AOCApp
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.Stream

object Day8 extends AOCApp {
  override def inputFileName: String = "2024_8.txt"

  case class Antenna(x: Int, y: Int, freq: Char)

  case class Antinode(x: Int, y: Int)

  def antinodes
  (
    a1: Antenna,
    a2: Antenna,
    inBounds: (Int, Int) => Boolean = (_, _) => false
  ): List[Antinode] = {
    val dy = a2.y - a1.y
    val dx = a2.x - a1.x

    def nodes(n: Int) =
      List(Antinode(a1.x - n * dx, a1.y - n * dy), Antinode(a2.x + n * dx, a2.y + n * dy))

    var allNodes = List.empty[Antinode]
    var resonate = true
    var resonance = 1
    while(resonate) {
      val resNodes = nodes(resonance)
      allNodes = allNodes ++ resNodes
      resonate = resNodes.exists(n => inBounds(n.x, n.y))
      resonance += 1
    }
    allNodes
  }

  override def part1: Stream[IO, Unit] = {
    for {
      grid <- Stream.eval(Ref[IO].of(Grid.empty))
      antennae <- Stream.eval(Ref[IO].of(Set.empty[Antenna]))
      _ <- input
        .evalMap { line =>
          grid
            .updateAndGet(_.withRow(line))
            .flatMap { grid =>
              antennae.update { antennae =>
                line
                  .zipWithIndex
                  .filter(_._1 != '.')
                  .foldLeft(antennae) { case (antennae, (c, i)) =>
                    antennae + Antenna(grid.nRows - 1, i, c)
                  }
              }
            }
        }
        .onFinalize {
          (grid.get, antennae.get).flatMapN((grid, antennae) =>
            val nodes = antennae
              .toList
              .combinations(2)
              .toList
              .filter(pair => pair.head.freq == pair.last.freq)
              .flatMap(pair =>
                val nodes = antinodes(pair.head, pair.last)
                nodes
              )
              .filter(node => grid(node.x, node.y).isDefined)
              .distinct
            IO.println(s"${nodes.size}")// >> IO.println(s"${nodes.mkString("\n")}")
          )
        }
    } yield ()
  }

  override def part2: Stream[IO, Unit] = {
    for {
      grid <- Stream.eval(Ref[IO].of(Grid.empty))
      antennae <- Stream.eval(Ref[IO].of(Set.empty[Antenna]))
      _ <- input
        .evalMap { line =>
          grid
            .updateAndGet(_.withRow(line))
            .flatMap { grid =>
              antennae.update { antennae =>
                line
                  .zipWithIndex
                  .filter(_._1 != '.')
                  .foldLeft(antennae) { case (antennae, (c, i)) =>
                    antennae + Antenna(grid.nRows - 1, i, c)
                  }
              }
            }
        }
        .onFinalize {
          (grid.get, antennae.get).flatMapN((grid, antennae) =>
            val nodes = antennae
              .toList
              .combinations(2)
              .toList
              .filter(pair => pair.head.freq == pair.last.freq)
              .flatMap(pair =>
                val nodes = antinodes(pair.head, pair.last, grid(_, _).isDefined)
                nodes ++ pair.map(a => Antinode(a.x, a.y))
              )
              .filter(node => grid(node.x, node.y).isDefined)
              .distinct
            IO.println(s"${nodes.size}")// >> IO.println(s"${nodes.mkString("\n")}")
          )
        }
    } yield ()
  }
}
