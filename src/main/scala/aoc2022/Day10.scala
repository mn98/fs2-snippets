package aoc2022

import aoc.AOCApp
import cats.effect.std.Queue
import cats.effect.{Concurrent, IO, Ref}
import cats.syntax.all.*
import fs2.Stream

import scala.util.matching.Regex

object Day10 extends AOCApp {

  override def inputFileName: String = "AOC_2022_10.txt"

  enum Instruction:
    case Noop
    case Add(x: Int)

  object Instruction:
    val noopPattern: Regex = "noop".r
    val addxPattern: Regex = "addx (-?[0-9]+)".r

  private def consumeInstructions(consume: Instruction => IO[Unit]): Stream[IO, Unit] =
    input
      .filter(_.nonEmpty)
      .evalMap {
        case Instruction.noopPattern() =>
          consume(Instruction.Noop)
        case Instruction.addxPattern(x) =>
          consume(Instruction.Noop) >> consume(Instruction.Add(x.toInt))
      }

  private def processInstructions(
                                   instructions: Stream[IO, Instruction],
                                   execute: (Int, Int) => IO[Unit]
                                 ): Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(0)).flatMap { cycle =>
      Stream.eval(Ref[IO].of(1)).flatMap { x_register =>
        instructions
          .evalTap { _ =>
            cycle
              .updateAndGet(_ + 1)
              .flatMap { cycle =>
                x_register.get.flatMap { x =>
                  IO.println(s"X is $x during cycle $cycle") >> execute(cycle, x)
                }
              }
          }
          .evalMap {
            case Instruction.Noop => IO.unit
            case Instruction.Add(x) => x_register.update(_ + x)
          }
      }
    }

  override def part1: Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(0L)).flatMap { signalStrength =>
      Stream.eval(Queue.unbounded[IO, Option[Instruction]]).flatMap { instructions =>
        Stream(
          consumeInstructions(i => instructions.offer(Some(i))).onFinalize(instructions.offer(None)),
          processInstructions(
            Stream.fromQueueNoneTerminated(instructions),
            (cycle, x) =>
              signalStrength
                .update(_ + x * cycle)
                .whenA(cycle <= 220 && ((cycle + 20) % 40) == 0)
          )
        )
          .parJoinUnbounded
          .onFinalize {
            signalStrength.get.flatMap { signalStrength =>
              IO.println(s"Sum of signal strengths is $signalStrength")
            }
          }
      }
    }

  override def part2: Stream[IO, Unit] =
    Stream.eval(Ref[IO].of("")).flatMap { screen =>
      Stream.eval(Queue.unbounded[IO, Option[Instruction]]).flatMap { instructions =>
        Stream(
          consumeInstructions(i => instructions.offer(Some(i))).onFinalize(instructions.offer(None)),
          processInstructions(
            Stream.fromQueueNoneTerminated(instructions),
            (cycle, x) =>
              screen
                .update { screen =>
                  s"$screen${if math.abs(x - ((cycle % 40) - 1)) > 1 then "." else "#"}"
                }
          )
        )
          .parJoinUnbounded
          .onFinalize {
            screen.get.flatMap { screen =>
              IO.println(screen.grouped(40).mkString("\n"))
            }
          }
      }
    }

}
