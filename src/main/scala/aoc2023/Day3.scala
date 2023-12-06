package aoc2023

import aoc.AOCApp
import cats.effect.IO
import cats.effect.kernel.Ref
import fs2.Stream

import scala.util.matching.Regex

object Day3 extends AOCApp {

  override def inputFileName: String = "AOC_2023_3.txt"

  val number: Regex = "([0-9]+)".r
  val symbol: Regex = "([^0-9.])".r
  val star: Regex = "([*])".r

  case class Number(number: Int, index: Int):
    private val end = index + number.toString.length

    def overlaps(i: Int): Boolean = i >= index - 1 && i <= end

  case class Symbol(symbol: String, index: Int)

  case class Data(numbers: Seq[Number], symbols: Seq[Symbol])

  case class Gear(number1: Number, number2: Number):
    def power: Long = number1.number * number2.number

  override def part1: Stream[IO, Unit] =
    Stream.eval(Ref.of[IO, List[Int]](Nil)).flatMap { partNumbers =>
      Stream.eval(Ref.of[IO, List[Data]](Nil)).flatMap { data =>
        input
          .evalMap { input =>
            val numbers = number.findAllMatchIn(input).toSeq.map(m => Number(m.matched.toInt, m.start))
            val symbols = symbol.findAllMatchIn(input).toSeq.map(m => Symbol(m.matched, m.start))
            IO.println(s"$input\n$numbers\n$symbols") >>
              data.update(_.appended(Data(numbers, symbols)))
          }
          .onFinalize {
            data.get.flatMap { data =>
              val parts = data.zipWithIndex.flatMap { (row, i) =>
                val previousRow = if i > 0 then Some(data(i - 1)) else None
                val nextRow = if i < data.size - 1 then Some(data(i + 1)) else None
                val parts = row.numbers.filter { number =>
                  val previousSymbols = previousRow.map(_.symbols).toSeq.flatten
                  val nextSymbols = nextRow.map(_.symbols).toSeq.flatten
                  val symbols = row.symbols ++ previousSymbols ++ nextSymbols
                  symbols.exists { symbol =>
                    number.overlaps(symbol.index)
                  }
                }
                println(s"Parts on row $i: $parts")
                parts
              }
              IO.println(s"Sum of part numbers: ${parts.map(_.number).sum}")
            }
          }
      }
    }

  override def part2: Stream[IO, Unit] =
    Stream.eval(Ref.of[IO, List[Int]](Nil)).flatMap { partNumbers =>
      Stream.eval(Ref.of[IO, List[Data]](Nil)).flatMap { data =>
        input
          .evalMap { input =>
            val numbers = number.findAllMatchIn(input).toSeq.map(m => Number(m.matched.toInt, m.start))
            val symbols = star.findAllMatchIn(input).toSeq.map(m => Symbol(m.matched, m.start))
            IO.println(s"$input\n$numbers\n$symbols") >>
              data.update(_.appended(Data(numbers, symbols)))
          }
          .onFinalize {
            data.get.flatMap { data =>
              val gears = data.zipWithIndex.flatMap { (row, i) =>
                val previousRow = if i > 0 then Some(data(i - 1)) else None
                val nextRow = if i < data.size - 1 then Some(data(i + 1)) else None
                val gears = row.symbols.flatMap { symbol =>
                  val previousNumbers = previousRow.map(_.numbers).toSeq.flatten
                  val nextNumbers = nextRow.map(_.numbers).toSeq.flatten
                  val numbers = row.numbers ++ previousNumbers ++ nextNumbers
                  numbers.filter(_.overlaps(symbol.index)) match {
                    case Seq(n1, n2) => Some(Gear(n1, n2))
                    case _ => None
                  }
                }
                println(s"Gears on row $i: $gears")
                gears
              }
              IO.println(s"Sum of gear powers: ${gears.map(_.power).sum}")
            }
          }
      }
    }

}

