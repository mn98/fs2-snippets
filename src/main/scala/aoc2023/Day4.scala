package aoc2023

import aoc.AOCApp
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import fs2.Stream

import scala.util.matching.Regex

object Day4 extends AOCApp {

  override def inputFileName: String = "AOC_2023_4.txt"

  val pattern: Regex = "Card\\s+([0-9]+): (.+) \\| (.+)".r
  val number: Regex = "([0-9]+)".r

  override def part1: Stream[IO, Unit] =
    Stream.eval(Ref.of[IO, Long](0)).flatMap { sum =>
      input
        .evalMap {
          case pattern(card, winners, scratched) =>
            val winningNumbers = number.findAllIn(winners).map(_.toInt).toSet
            val scratchedNumbers = number.findAllIn(scratched).map(_.toInt).toSet
            val matches = (winningNumbers intersect scratchedNumbers).size
            val score = if matches > 0 then 1L << (matches - 1) else 0
            IO.println(s"$card, $winners, $scratched, $matches, $score") >> sum.update(_ + score)
        }
        .onFinalize {
          sum.get.flatMap(sum => IO.println(s"Sum is $sum"))
        }
    }

  override def part2: Stream[IO, Unit] =
    Stream.eval(Ref.of[IO, Map[Int, Long]](Map.empty)).flatMap { counter =>
      Stream.eval(Ref.of[IO, Int](0)).flatMap { lastCard =>
        input
          .evalMap {
            case pattern(card, winners, scratched) =>
              val cardNumber = card.toInt
              val winningNumbers = number.findAllIn(winners).map(_.toInt).toSet
              val scratchedNumbers = number.findAllIn(scratched).map(_.toInt).toSet
              val matches = (winningNumbers intersect scratchedNumbers).size
              IO.println(s"$winningNumbers, $scratchedNumbers, $matches") >>
                lastCard.set(cardNumber) >>
                counter.get.map(_.getOrElse(cardNumber, 0L) + 1).flatMap { multiplier =>
                  counter.update { m =>
                    (cardNumber to cardNumber + matches).foldLeft(m) { (m, n) =>
                      val increment = if n > cardNumber then multiplier else 1
                      m.updated(n, m.getOrElse(n, 0L) + increment)
                    }
                  }
                }
          }
          .onFinalize {
            (lastCard.get, counter.get).flatMapN { (lastCard, counter) =>
              IO.println(s"Counted $counter\nTotal ${
                counter.view.filterKeys(_ <= lastCard).values.sum
              }")
            }
          }
      }
    }

}
