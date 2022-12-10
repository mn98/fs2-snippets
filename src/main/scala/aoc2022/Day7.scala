package aoc2022

import aoc.AOCApp
import cats.effect.kernel.Concurrent
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.Stream

import scala.util.matching.Regex

object Day7 extends AOCApp {

  override def inputFileName: String = "AOC_2022_7.txt"

  object TermPattern {
    val cd: Regex = "\\$ cd (.*)".r
    val ls: Regex = "\\$ ls".r
    val dir: Regex = "dir ([a-z]+)".r
    val file: Regex = "([0-9]+) (.*)".r
  }

  trait FSPath[F[_]] {
    def directories: F[List[String]]

    def path: F[String]

    def paths: F[List[String]]

    def up: F[Unit]

    def down(dir: String): F[Unit]
  }

  object FSPath {
    def empty[F[_] : Concurrent]: F[FSPath[F]] =
      Ref.of[F, List[String]](Nil).map { dirs =>
        new FSPath[F] {
          override def directories: F[List[String]] = dirs.get

          override def path: F[String] = dirs.get.map(_.mkString("/"))

          override def paths: F[List[String]] = dirs.get.map { dirs =>
            (for i <- 1 to dirs.size yield dirs.take(i).mkString("/")).toList
          }

          override def up: F[Unit] = dirs.update(_.dropRight(1))

          override def down(dir: String): F[Unit] = dirs.update(_ :+ dir)
        }
      }
  }

  private def readDirectorySizes(sizes: Ref[IO, Map[String, Long]]): IO[Unit] =
    FSPath.empty[IO].flatMap { path =>
      input.filter(_.nonEmpty).evalMap {
        case TermPattern.cd(to) =>
          if to == ".." then path.up else path.down(to)

        case TermPattern.ls() =>
          IO.unit

        case TermPattern.dir(_) =>
          IO.unit

        case TermPattern.file(size, _) =>
          path.paths.flatMap { paths =>
            sizes.update { sizes =>
              paths.foldLeft(sizes) { (sizes, path) =>
                sizes.updated(path, sizes.getOrElse(path, 0L) + size.toLong)
              }
            }
          }

        case unknown =>
          IO.println(s"Could not match: $unknown")

      }
        .compile
        .drain
    }

  override def part1: Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(Map.empty[String, Long])).flatMap { sizes =>
      Stream.eval(readDirectorySizes(sizes))
        .onFinalize {
          sizes.get.flatMap { sizes =>
            val sum = sizes
              .filter { case (_, size) => size <= 100000 }
              .values
              .sum
            IO.println(s"Directory sizes\n${
              sizes.toSeq.sortBy(_._2).mkString("\n")
            }") >>
              IO.println(s"Sum of directories <= 100,000 is $sum")
          }
        }
    }

  private val totalSpace = 70_000_000
  private val freeSpaceRequired = 30_000_000

  override def part2: Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(Map.empty[String, Long])).flatMap { sizes =>
      Stream.eval(readDirectorySizes(sizes))
        .onFinalize {
          sizes.get.flatMap { sizes =>
            val usedSpace = sizes("/")
            val freeSpace = totalSpace - usedSpace
            val spaceToFreeUp = freeSpaceRequired - freeSpace
            val directoryToDelete =
              sizes
                .toSeq
                .sortBy(_._2)
                .dropWhile(_._2 <= spaceToFreeUp)
                .head
            IO.println(s"Used space is $usedSpace") >>
              IO.println(s"To free $spaceToFreeUp, delete $directoryToDelete")
          }
        }
    }

}
