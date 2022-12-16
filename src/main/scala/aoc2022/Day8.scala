package aoc2022

import aoc.AOCApp
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all.*
import fs2.Stream

import scala.collection.immutable.Seq

object Day8 extends AOCApp {

  override def inputFileName: String = "AOC_2022_8.txt"

  case class Forest(rows: Seq[Seq[Int]]) {
    val columns: Seq[Seq[Int]] = {
      if rows.isEmpty then Seq.empty
      else rows.head.indices.map(i => rows.map(_ (i)))
    }
  }

  private def buildForest(forest: Ref[IO, Forest]): IO[Unit] =
    input
      .filter(_.nonEmpty)
      .map(_.toCharArray.toSeq.map(_.toString.toInt))
      .evalTap { row =>
        forest.update(forest => Forest(forest.rows :+ row))
      }
      .compile
      .drain

  private def visibleTrees(lineOfSight: Seq[Int]): Set[Int] = {
    def visibleIndices(los: Seq[(Int, Int)]): Set[Int] = {
      var maxHeight = -1
      los
        .collect {
          case (height, i) if height > maxHeight =>
            maxHeight = height
            i
        }
        .toSet
    }

    visibleIndices(lineOfSight.zipWithIndex) ++
      visibleIndices(lineOfSight.zipWithIndex.reverse)
  }

  override def part1: Stream[IO, Unit] =
    Stream.eval {
      Ref[IO].of(Forest(Seq.empty)).flatMap { forest =>
        buildForest(forest) >>
          forest.get.flatMap { forest =>
            val visibleRowTrees = forest.rows.zipWithIndex.foldLeft(Set.empty[(Int, Int)]) {
              case (trees, (row, r)) => trees ++ visibleTrees(row).map(c => (r, c))
            }
            val visibleColTrees = forest.columns.zipWithIndex.foldLeft(Set.empty[(Int, Int)]) {
              case (trees, (col, c)) => trees ++ visibleTrees(col).map(r => (r, c))
            }
            val allVisibleTrees = visibleRowTrees ++ visibleColTrees
            IO.println(s"Visible trees: ${allVisibleTrees.size}")
          }
      }
    }

  override def part2: Stream[IO, Unit] =
    Stream.eval {
      Ref[IO].of(Forest(Seq.empty)).flatMap { forest =>
        buildForest(forest) >>
          forest.get.flatMap { forest =>
            def scenicScore(lineOfSight: Seq[Int]): Int = {
              val score = if lineOfSight.size == 1 then 0 else {
                math.min(
                  lineOfSight.tail.takeWhile(_ < lineOfSight.head).size + 1,
                  lineOfSight.size - 1
                )
              }
              score
            }

            var highestScenicScore = 0
            for {
              (row, i) <- forest.rows.zipWithIndex.drop(1).dropRight(1)
              (column, j) <- forest.columns.zipWithIndex.drop(1).dropRight(1)
            } do {
              val left: Int = scenicScore(row.take(j + 1).reverse)
              val right: Int = scenicScore(row.drop(j))
              val up: Int = scenicScore(column.take(i + 1).reverse)
              val down: Int = scenicScore(column.drop(i))
              val score = left * right * up * down
              if score > highestScenicScore then highestScenicScore = score
            }
            // 37800 is too low
            IO.println(s"Highest scenic score is $highestScenicScore")
          }
      }

    }
}