package aoc2024

import aoc.AOCApp
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.Stream

import scala.util.matching.Regex

object Day4 extends AOCApp {
  override def inputFileName: String = "2024_4.txt"

  val xmas: Regex = "X(?=MAS)|S(?=AMX)".r
  val mas: Regex = "MAS|SAM".r

  opaque type Grid = List[String]

  object Grid:
    def empty: Grid = List.empty

  extension (grid: Grid)
    def withRow(row: String): Grid = grid.appended(row)
    def nRows: Int = grid.size
    def nCols: Int = grid.head.length
    def rows: List[String] = grid
    def cols: List[String] = grid.transpose.map(_.mkString)
    def apply(i: Int, j: Int): Option[Char] =
      rows.get(i).flatMap(s => if (j >= 0 && j < s.length) Some(s.charAt(j)) else None)

  override def part1: Stream[IO, Unit] =
    for {
      grid <- Stream.eval(Ref[IO].of(Grid.empty))
      _ <- input
        .evalMap { line =>
          grid.update(_.withRow(line))
        }
        .onFinalize {
          grid.get.flatMap { grid =>
            val rows = grid.rows
            val cols = grid.cols
            var count: Long = 0
            for {
              i <- rows.indices
              j <- cols.indices
            } do {
              if (grid(i)(j) == 'X') {
                List(
                  (-3 to 3).flatMap(o => grid(i, j + o)).mkString,
                  (-3 to 3).flatMap(o => grid(i + o, j)).mkString,
                  (-3 to 3).flatMap(o => grid(i + o, j + o)).mkString,
                  (-3 to 3).flatMap(o => grid(i + o, j - o)).mkString
                )
                  .foreach(count += xmas.findAllIn(_).size)
              }
            }
            IO.println(s"Count = $count")
          }
        }
    } yield ()

  override def part2: Stream[IO, Unit] =
    for {
      grid <- Stream.eval(Ref[IO].of(Grid.empty))
      _ <- input
        .evalMap { line =>
          grid.update(_.withRow(line))
        }
        .onFinalize {
          grid.get.flatMap { grid =>
            val rows = grid.rows
            val cols = grid.cols
            var count: Long = 0
            for {
              i <- rows.indices
              j <- cols.indices
            } do {
              if (grid(i)(j) == 'A') {
                if(List(
                  (-1 to 1).flatMap(o => grid(i + o, j + o)).mkString,
                  (-1 to 1).flatMap(o => grid(i + o, j - o)).mkString
                )
                  .forall(mas.findAllIn(_).size == 1)) count += 1
              }
            }
            IO.println(s"Count = $count")
          }
        }
    } yield ()

}
