package aoc2022

import aoc.AOCApp
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.Stream

import scala.collection.immutable.SortedMap
import scala.util.matching.Regex

object Day5 extends AOCApp {

  override def inputFileName: String = "AOC_2022_5.txt"

  type Stacks = SortedMap[Int, List[Char]]
  private val emptyStacks: Stacks = SortedMap.empty
  extension (stacks: Stacks) {
    def addCrate(index: Int, crate: Char): Stacks = {
      val stackToUpdate = if (stacks.isDefinedAt(index)) stacks(index) else Nil
      stacks.updated(index, crate :: stackToUpdate)
    }
    def moveCrates(move: Move, multipleCratesAtOnce: Boolean): Stacks = {
      val cratesToMove = stacks(move.from).takeRight(move.quantity)
      val arrangedCratesToMove = if multipleCratesAtOnce then cratesToMove else cratesToMove.reverse
      stacks
        .updated(move.from, stacks(move.from).dropRight(move.quantity))
        .updated(move.to, stacks(move.to) appendedAll arrangedCratesToMove)
    }
  }

  private val cratePattern: Regex = "\\[([A-Z])]\\s*".r

  case class Move(quantity: Int, from: Int, to: Int)

  object Move {
    private val description: Regex = "move ([0-9]+) from ([0-9]) to ([0-9])".r

    def parse(s: String): Move = s match {
      case description(quantity, from, to) => Move(quantity.toInt, from.toInt - 1, to.toInt - 1)
      case _ => throw new IllegalArgumentException(s"Could not parse: $s")
    }
  }

  override def part1: Stream[IO, Unit] = rearrangeCrates(false)

  override def part2: Stream[IO, Unit] = rearrangeCrates(true)

  private def rearrangeCrates(moveMultipleCratesAtOnce: Boolean): Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(emptyStacks)).flatMap { stacks =>
      input
        .filter(_.nonEmpty)
        .map(_.split("(?<=\\G.{3} )").toList)
        .evalMap { lineElements =>
          stacks.update { stacks =>
            if (lineElements.size > 1) // building up the stacks
              lineElements.zipWithIndex.foldLeft(stacks) { case (stacks, (crate, index)) =>
                crate match {
                  case cratePattern(c) =>
                    stacks.addCrate(index, c.charAt(0))
                  case _ =>
                    stacks
                }
              }
            else // modifying the stacks
              val move = Move.parse(lineElements.head)
              stacks.moveCrates(move, moveMultipleCratesAtOnce)
          }
        }
        .onFinalize {
          stacks.get.flatMap { stacks =>
            IO.println(s"Stacks\n======\n${stacks.mkString("\n")}") >>
              IO.println(s"Top crates: ${stacks.values.flatMap(_.lastOption).mkString}")
          }
        }

    }


}
