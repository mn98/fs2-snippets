package aoc2022

import aoc.AOCApp
import cats.effect.{IO, Ref}
import fs2.Stream

import scala.util.matching.Regex

object Day9 extends AOCApp {

  override def inputFileName: String = "AOC_2022_9.txt"

  enum Direction:
    case Left, Right, Up, Down
  end Direction

  object Direction:
    def from(c: Char): Direction = c match {
      case 'L' => Left
      case 'R' => Right
      case 'U' => Up
      case 'D' => Down
    }
  end Direction

  case class Knot(i: Int, j: Int) {
    def move(m: Direction): Knot = m match {
      case Direction.Left => Knot(i, j - 1)
      case Direction.Right => Knot(i, j + 1)
      case Direction.Up => Knot(i + 1, j)
      case Direction.Down => Knot(i - 1, j)
    }

    def catchUpWith(knot: Knot): Knot = {
      val h = knot.j - j
      val v = knot.i - i
      val h_dist = math.abs(h)
      val v_dist = math.abs(v)
      val h_move_size =
        if v_dist > 1 then math.min(h_dist, 1)
        else math.max(h_dist - 1, 0)
      val v_move_size =
        if h_dist > 1 then math.min(v_dist, 1)
        else math.max(v_dist - 1, 0)

      Knot(
        i + v_move_size * math.signum(v),
        j + h_move_size * math.signum(h)
      )
    }
  }

  case class Rope(knots: Seq[Knot]) {
    def move(m: Direction): Rope = Rope {
      knots.tail.foldLeft(Seq(knots.head.move(m))) { case (knots, knot) =>
        knots :+ knot.catchUpWith(knots.last)
      }
    }
  }

  case class Move(direction: Direction, distance: Int)

  object Move:
    val pattern: Regex = "([A-Z]) ([0-9]+)".r

  private val start: Knot = Knot(0, 0)

  override def part1: Stream[IO, Unit] = go(2)

  // 2528 is too low
  override def part2: Stream[IO, Unit] = go(10)

  private def go(numberOfKnots: Int): Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(Rope(Seq.fill(numberOfKnots)(start)))).flatMap { rope =>
      Stream.eval(rope.get.flatMap(r => Ref[IO].of(Set(r.knots.last)))).flatMap { visited =>
        input
          .filter(_.nonEmpty)
          .map {
            case Move.pattern(direction, distance) =>
              Move(Direction.from(direction.head), distance.toInt)
          }
          .flatMap { move =>
            Stream
              .eval {
                rope
                  .updateAndGet(_.move(move.direction))
                  .flatMap(rope => visited.update(_ + rope.knots.last))
              }
              .repeatN(move.distance)
          }
          .onFinalize {
            visited.get.flatMap { visited =>
              IO.println(s"Rope's tail visited ${visited.size} distinct positions")
            }
          }
          .drain
      }
    }

}
