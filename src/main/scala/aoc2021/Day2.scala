package aoc2021

import cats.effect.{IO, IOApp, Ref}
import cats.syntax.all.*
import fs2.io.file.{Files, Flags, Path}
import fs2.{Stream, text}

object Day2 extends IOApp.Simple {

  case class Command(direction: Command.Direction.Value, distance: Int)

  object Command {
    object Direction extends Enumeration {
      val forward, down, up = Value
    }
  }

  val input: fs2.Stream[IO, Command] =
    Files[IO]
      .readAll(Path(getClass.getResource("/aoc/AOC_2_input.txt").getPath), 1024, Flags.Read)
      .through(text.utf8.decode andThen text.lines)
      .filterNot(_.isEmpty)
      .map { line =>
        line.split(" ").toList match {
          case direction :: distance :: Nil =>
            Command(Command.Direction.withName(direction.toLowerCase), distance.toInt)
        }
      }

  case class Position(horizontal: Int, depth: Int, aim: Int)

  val program1: Stream[IO, Unit] =
    Stream.eval(Ref.of[IO, Position](Position(0, 0, 0))).flatMap { position =>
      input.evalMap { command =>
          command.direction match {
            case Command.Direction.forward => position.update(p => p.copy(horizontal = p.horizontal + command.distance))
            case Command.Direction.up => position.update(p => p.copy(depth = p.depth - command.distance))
            case Command.Direction.down => position.update(p => p.copy(depth = p.depth + command.distance))
          }
        }
        .onFinalize(position.get.flatMap { p =>
          IO(println(s"Position is $p with a product of ${p.horizontal * p.depth}"))
        })
    }

  val program2: Stream[IO, Unit] =
    Stream.eval(Ref.of[IO, Position](Position(0, 0, 0))).flatMap { position =>
      input.evalMap { command =>
          command.direction match {
            case Command.Direction.forward => position.update(p =>
              p.copy(
                horizontal = p.horizontal + command.distance,
                depth = p.depth + p.aim * command.distance
              )
            )
            case Command.Direction.up => position.update(p => p.copy(aim = p.aim - command.distance))
            case Command.Direction.down => position.update(p => p.copy(aim = p.aim + command.distance))
          }
        }
        .onFinalize(position.get.flatMap { p =>
          IO(println(s"Position is $p with a product of ${p.horizontal * p.depth}"))
        })
    }

  override def run: IO[Unit] = (program1 ++ program2).compile.drain

}
