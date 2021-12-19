package aoc

import cats.effect.{IO, ParallelF}
import cats.effect.kernel.{Concurrent, Ref}
import cats.syntax.all._
import fs2.Stream

object Day10 extends AOCApp {

  override def inputFileName: String = "AOC_10_input.txt"

  val lines: Stream[IO, String] = input.takeWhile(_.nonEmpty) //.take(1)

  trait TokenParser[F[_]] {
    def parse(token: Char): F[Boolean]

    def chunks: F[Seq[String]]

    def lastToken: F[Option[Char]]

    def lastChunk: F[Option[String]]

    def autoCompletion: F[Seq[Char]]
  }

  object TokenParser {

    private val openers = "(<{["
    private val closers = ")>}]"
    private val openerForCloser = Map(')' -> '(', '}' -> '{', ']' -> '[', '>' -> '<')
    private val closerForOpener = openerForCloser.map { case (k, v) => (v, k) }

    val invalidTokenScores = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)

    val autoCompleteTokenScores = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

    def apply[F[_] : Concurrent]: F[TokenParser[F]] = {
      for {
        chunkHistory <- Ref[F].of(Seq.empty[String])
        tokenStack <- Ref[F].of(Seq.empty[Char])
      } yield {
        new TokenParser[F] {

          override def parse(token: Char): F[Boolean] = {
            tokenStack.modify { chunkStack =>
              if (openers.contains(token)) (chunkStack :+ token, true)
              else {
                chunkStack.lastOption match {
                  case Some(previousToken) =>
                    if (previousToken == openerForCloser(token)) (chunkStack.dropRight(1), true)
                    else (chunkStack, false)
                  case None => (chunkStack, false)
                }
              }
            }
          }

          override def chunks: F[Seq[String]] = chunkHistory.get

          override def lastToken: F[Option[Char]] = tokenStack.get.map(_.lastOption)

          override def lastChunk: F[Option[String]] = chunkHistory.get.map(_.lastOption)

          override def autoCompletion: F[Seq[Char]] = tokenStack.get.map(_.reverse.map(closerForOpener))
        }
      }
    }
  }

  override def part1: Stream[IO, Unit] = {
    Stream.eval(Ref[IO].of(Seq.empty[Seq[Char]])).flatMap { autoCompletions =>
      Stream.eval(Ref[IO].of(Seq.empty[Char])).flatMap { failedTokens =>
        lines.evalMap { line =>
          val lineParser = Stream.eval(TokenParser[IO]).flatMap { tokenParser =>
            Stream
              .emits(line.toIndexedSeq)
              .evalMap { token =>
                tokenParser.parse(token)
                  .flatTap(result =>
                    (IO(println(s"Failed at $token")) >> failedTokens.update(_ :+ token)).whenA(!result)
                  )
                  .map(result => token -> result)
              }
              .takeWhile({ case (_, result) => result }, true)
              .onFinalize {
                tokenParser.autoCompletion.flatMap { autoCompletion =>
                  autoCompletions.update(_.appended(autoCompletion))
                }
              }
          }
          lineParser.compile.drain
        }
          .onFinalize {
            failedTokens.get.flatMap { failedTokens =>
              val score = failedTokens.map(TokenParser.invalidTokenScores).sum
              IO(println(s"Failed tokens (score = $score): $failedTokens"))
            } >>
              autoCompletions.get.flatMap { autoCompletions =>
                val scores = autoCompletions.map { autoCompletion =>
                  autoCompletion.map(TokenParser.autoCompleteTokenScores).sum
                }
                scores.sorted.get(scores.size * 2 - 1).traverse_ { score =>
                  IO(println(s"Autocompletion winning score is $score"))
                }
              }
          }
      }
    }
  }

  override def part2: Stream[IO, Unit] = Stream.empty

}
