package aoc2021

import cats.effect.kernel.Ref
import cats.effect.{IO, IOApp}
import cats.syntax.all._
import fs2.concurrent.SignallingRef
import fs2.io.file.{Files, Flags, Path}
import fs2.{Stream, text}

import java.nio.file.Paths

object Day4 extends IOApp.Simple {

  case class Bingo(numbers: Seq[Int], boards: Set[Bingo.Board])

  object Bingo {
    case class Board(rows: Seq[Seq[Int]]) {
      val columns: Seq[Seq[Int]] = rows.indices.map(i => rows.map(_ (i)))
      val lines: Seq[Seq[Int]] = rows ++ columns
    }
  }

  def check(board: Bingo.Board, seenNumbers: Seq[Int]): Option[Int] = {
    (board.rows ++ board.columns)
      .find(_.forall(seenNumbers.contains))
      .map(_ => (board.rows.flatten.toSet -- seenNumbers).sum * seenNumbers.last)
  }

  val input: Stream[IO, String] = Files[IO]
    .readAll(Path.fromNioPath(Paths.get(s"${System.getenv("HOME")}/Documents/AOC_4_input.txt")), 1024, Flags.Read)
    .through(text.utf8.decode andThen text.lines)

  val program: Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(Bingo(Seq.empty, Set.empty))).flatMap { bingo =>

      val setup =
        input.take(1).evalMap { csvNumbers =>
          bingo.update(_.copy(numbers = csvNumbers.split(",").map(_.toInt).toSeq))
        } ++
          input.drop(1)
            .groupAdjacentBy(_.isEmpty)
            .filterNot(_._1)
            .debug()
            .evalMap { case (_, rows) =>
              val numbers = rows.toList.map(row => row.trim.split(" +").map(_.toInt).toSeq)
              val board = Bingo.Board(numbers)
              bingo.update(bingo => bingo.copy(boards = bingo.boards + board))
            } ++
          Stream.exec(bingo.get.flatMap(bingo => IO(println(s"Bingo game\n$bingo"))))

      val play = {
        Stream.eval(bingo.get).flatMap { bingo =>
          Stream.eval(SignallingRef[IO].of(false)).flatMap { gameOver =>
            Stream.eval(Ref[IO].of(bingo.boards)).flatMap { boardsPlaying =>
              Stream.eval(Ref[IO].of(Seq.empty[Int])).flatMap { calledNumbers =>
                Stream.emits(bingo.numbers).evalMap { number =>
                  calledNumbers
                    .updateAndGet(_ :+ number)
                    .flatMap { calledNumbers =>
                      boardsPlaying.modify { boardsPlaying =>
                        val winners = boardsPlaying
                          .map(board => board -> check(board, calledNumbers))
                          .filter(_._2.isDefined)
                        (boardsPlaying -- winners.map(_._1), winners.map(_._2))
                      }
                        .flatMap { scores =>
                          IO(println(s"Scores at $number: ${scores.flatten}")) >>
                            boardsPlaying.get.flatMap { boardsPlaying =>
                              gameOver.set(true).whenA(boardsPlaying.isEmpty)
                            }
                        }
                    }
                }
                  .interruptWhen(gameOver)
                  .onFinalize(calledNumbers.get.flatMap(numbers =>
                    IO(println(s"Game over!\nCalled numbers: $numbers"))
                  ))
              }
            }
          }
        }
      }

      setup ++ play
    }

  override def run: IO[Unit] = program.compile.drain

}
