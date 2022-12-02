package aoc2022

import aoc.AOCApp
import cats.effect.IO
import cats.effect.kernel.Ref
import fs2.Stream

object Day2 extends AOCApp {

  override def inputFileName: String = "AOC_2022_2.txt"

  enum Move(val points: Int):
    case Rock extends Move(1)
    case Paper extends Move(2)
    case Scissors extends Move(3)

  extension (move: Move) {

    def plays(otherMove: Move): Result = (move, otherMove) match {
      case (Move.Rock, Move.Scissors) => Result.Win
      case (Move.Paper, Move.Rock) => Result.Win
      case (Move.Scissors, Move.Paper) => Result.Win
      case (m, o) if m == o => Result.Draw
      case _ => Result.Lose
    }

    def requiresToEndIn(result: Result): Move = result match {
      case Result.Draw => move
      case Result.Win => move match {
        case Move.Rock => Move.Paper
        case Move.Paper => Move.Scissors
        case Move.Scissors => Move.Rock
      }
      case Result.Lose => move match {
        case Move.Rock => Move.Scissors
        case Move.Paper => Move.Rock
        case Move.Scissors => Move.Paper
      }
    }

  }

  enum Result(val points: Int):
    case Win extends Result(6)
    case Draw extends Result(3)
    case Lose extends Result(0)

  private val opponentsStrategy: Map[Char, Move] = Map(
    'A' -> Move.Rock,
    'B' -> Move.Paper,
    'C' -> Move.Scissors
  )

  private val yourMoveStrategy: Map[Char, Move] = Map(
    'X' -> Move.Rock,
    'Y' -> Move.Paper,
    'Z' -> Move.Scissors
  )

  private val yourResultStrategy: Map[Char, Result] = Map(
    'X' -> Result.Lose,
    'Y' -> Result.Draw,
    'Z' -> Result.Win
  )

  override def part1: Stream[IO, Unit] =
    play { (opponents, yours) =>
      (opponentsStrategy(opponents), yourMoveStrategy(yours))
    }

  override def part2: Stream[IO, Unit] =
    play { (opponents, yours) =>
      val opponentsMove = opponentsStrategy(opponents)
      val result = yourResultStrategy(yours)
      val yourMove = opponentsMove requiresToEndIn result
      (opponentsMove, yourMove)
    }

  def play(opponentsAndYourMoves: (Char, Char) => (Move, Move)): Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(0L)).flatMap { totalScore =>
      input
        .filter(_.nonEmpty)
        .evalMap { strategies =>
          val (opponentsMove, yourMove) = opponentsAndYourMoves(strategies(0), strategies(2))
          val result = yourMove plays opponentsMove
          totalScore.update(_ + yourMove.points + result.points)
        }
        .onFinalize {
          totalScore.get.flatMap { totalScore =>
            IO.println(s"Your total score is $totalScore")
          }
        }
    }

}
