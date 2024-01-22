package aoc2021

import aoc.AOCApp
import cats.effect.IO
import cats.effect.kernel.Ref
import fs2.Stream

object Day13 extends AOCApp {

  override def inputFileName: String = "AOC_13_input.txt"

  case class Paper(dots: Set[Paper.Point]) {

    override def toString: String = {
      val xMax = dots.maxBy(_.x).x
      val yMax = dots.maxBy(_.y).y
      (0 to yMax).map { y =>
        (0 to xMax).foldLeft("") { case (row, x) =>
          val dot = if (dots(Paper.Point(x, y))) "#" else "."
          s"$row$dot"
        }
      }.mkString("\n")
    }

    def fold(instruction: Instruction): Paper = {
      val i = instruction.index
      instruction.axis match {
        case Paper.Axis.X =>
          Paper(dots.map(dot => if (dot.x <= i) dot else dot.copy(x = dot.x - 2 * (dot.x - i))))
        case Paper.Axis.Y =>
          Paper(dots.map(dot => if (dot.y <= i) dot else dot.copy(y = dot.y - 2 * (dot.y - i))))
      }
    }
  }

  object Paper {
    case class Point(x: Int, y: Int)

    object Point {
      def apply(csv: String): Point = {
        csv.split(',').toList match {
          case x :: y :: Nil => Point(x.toInt, y.toInt)
          case _ => throw RuntimeException(s"Bad input: $csv")
        }
      }
    }

    object Axis extends Enumeration {
      val X, Y = Value
    }
  }

  case class Instruction(axis: Paper.Axis.Value, index: Int)

  def read(
            paper: Ref[IO, Paper],
            instructions: Ref[IO, Seq[Instruction]],
          ): Stream[IO, Unit] = {
    input
      .evalMap {
        case line if line.contains(",") =>
          paper.update(paper => paper.copy(dots = paper.dots + Paper.Point(line)))
        case line if line.startsWith("fold along") =>
          val instruction = line.stripPrefix("fold along ").split('=').toList match {
            case axis :: index :: Nil =>
              Instruction(Paper.Axis.withName(axis.toUpperCase), index.toInt)
            case _ => throw RuntimeException(s"Bad instruction: $line")
          }
          instructions.update(instructions => instructions :+ instruction)
        case _ =>
          IO.unit
      }
  }


  override def part1: Stream[IO, Unit] =
    for {
      paper <- Stream.eval(Ref[IO].of(Paper(Set.empty)))
      instructions <- Stream.eval(Ref[IO].of(Seq.empty[Instruction]))
      _ <- {
        read(paper, instructions) ++
          Stream.eval(paper.get.flatMap(paper => IO(println(s"Paper has ${paper.dots.size} dots")))) ++
          //Stream.eval(paper.get.flatMap(paper => IO(println(s"== Paper (${paper.dots.size}) ==\n$paper")))) ++
          Stream.eval(instructions.get).flatMap { instructions =>
            Stream
              .emits(instructions)
              .evalMap { instruction =>
                paper.update(_.fold(instruction))
              }
              .evalTap(_ => paper.get.flatMap(paper => IO(println(s"Paper has ${paper.dots.size} dots"))))
              //.evalTap(_ => paper.get.flatMap(paper => IO(println(s"== Paper (${paper.dots.size}) ==\n$paper"))))
              .onFinalize {
                paper.get.flatMap(paper => IO(println(s"== Paper (${paper.dots.size}) ==\n$paper")))
              }
          }
      }
    } yield ()

  override def part2: Stream[IO, Unit] = Stream.empty

}
