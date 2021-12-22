package aoc

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.kernel.Ref
import fs2.Stream

object Day12 extends AOCApp {

  override def inputFileName: String = "AOC_12_input.txt"

  case class Cave(name: String) {
    val isLarge: Boolean = name.forall(_.isUpper)
    val isSmall: Boolean = !isLarge

    override def toString: String = name
  }

  case class Tunnel(caves: (Cave, Cave)) {
    require(caves._1 != caves._2)

    override def toString: String = s"${caves._1}-${caves._2}"

    def connects(cave: Cave): Boolean = cave == caves._1 || cave == caves._2

    def caveSet: Set[Cave] = Set(caves._1, caves._2)
  }

  case class Path(caves: NonEmptyList[Cave]) {
    def visits(cave: Cave): Boolean = caves.exists(_ == cave)

    def connects(from: Cave, to: Cave): Boolean = caves.exists(_ == from) && caves.exists(_ == to)

    def append(cave: Cave): Path = Path(caves.append(cave))

    override def toString: String = caves.toList.mkString("-")
  }

  object Path {
    def apply(cave: Cave): Path = Path(NonEmptyList(cave, Nil))
  }

  case class CaveSystem(tunnels: Set[Tunnel]) {
    val caves: Set[Cave] = tunnels.flatMap(_.caveSet)
    val graph: Map[Cave, Set[Cave]] =
      caves.map { cave =>
        val connections = tunnels.filter(_.connects(cave)).flatMap(_.caveSet) - cave
        cave -> connections
      }.toMap

    def paths(
               from: Path,
               to: Cave,
               smallCaveCanBeVisited: (Path, Cave) => Boolean
             ): List[Path] = {
      graph
        .get(from.caves.last)
        .fold(List(from.caves.toList)) { caves =>
          caves.toList.collect {
            case cave if cave == to =>
              List(from.append(to))
            case cave if cave.isLarge =>
              paths(from.append(cave), to, smallCaveCanBeVisited)
            case cave if cave.isSmall && smallCaveCanBeVisited(from, cave) =>
              paths(from.append(cave), to, smallCaveCanBeVisited)
          }
            .flatten
            .map(_.caves.toList)
        }
        .map(caves => Path(NonEmptyList(caves.head, caves.tail)))
    }
  }

  def read(caveSystem: Ref[IO, CaveSystem]): Stream[IO, Unit] = {
    input
      .takeWhile(_.nonEmpty)
      .evalMap { line =>
        line.split("-").toList match {
          case cave1 :: cave2 :: Nil =>
            val tunnel = Tunnel((Cave(cave1), Cave(cave2)))
            caveSystem.update { caveSystem =>
              caveSystem.copy(tunnels = caveSystem.tunnels + tunnel)
            }
        }
      }
      .onFinalize {
        caveSystem.get.flatMap { caveSystem =>
          IO(println(s"Cave system has ${caveSystem.tunnels.size} tunnels connecting ${caveSystem.caves.size} caves")) >>
            IO(println(s"Caves: ${caveSystem.caves.mkString(", ")}")) >>
            IO(println(s"Tunnels:\n${caveSystem.tunnels.mkString("\n")}")) >>
            IO(println(s"Cave graph:\n${caveSystem.graph.mkString("\n")}"))
        }
      }
  }

  override def part1: Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(CaveSystem(Set.empty))).flatMap { caveSystem =>
      read(caveSystem) ++
        Stream.eval(caveSystem.get.flatMap { caveSystem =>
          val paths = caveSystem.paths(Path(Cave("start")), Cave("end"), (path, cave) => !path.visits(cave))
          IO(println(s"Found ${paths.size} paths")) //>>
          //IO(println(s"${paths.mkString("\n")}"))
        })
    }

  override def part2: Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(CaveSystem(Set.empty))).flatMap { caveSystem =>
      read(caveSystem) ++
        Stream.eval(caveSystem.get.flatMap { caveSystem =>
          val paths = caveSystem.paths(
            Path(Cave("start")),
            Cave("end"),
            (path, cave) => {
              val smallCaves = path.caves.toList.filter(_.isSmall)
              !path.visits(cave) || (
                smallCaves.distinct.size == smallCaves.size &&
                  !Set(Cave("start"), Cave("end)"))(cave)
                )
            }
          )
          IO(println(s"Found ${paths.size} paths")) //>>
          //IO(println(s"${paths.mkString("\n")}"))
        })
    }

}
