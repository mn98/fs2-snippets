package aoc2024

import cats.syntax.all.*

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
  def replaced(i: Int, j: Int, c: Char): Grid =
    grid.updated(i, grid(i).updated(j, c))
    