package aoc2024

import aoc.AOCApp
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.Stream

import scala.annotation.tailrec

object Day6 extends AOCApp {
  override def inputFileName: String = "2024_6.txt"

  case class Guard(r: Int, c: Int, o: Char) {
    def turn: Guard = copy(o = o match {
      case '^' => '>'
      case '>' => 'v'
      case 'v' => '<'
      case '<' => '^'
    })
    def move: Guard = o match {
      case '^' => copy(r = r - 1)
      case '>' => copy(c = c + 1)
      case 'v' => copy(r = r + 1)
      case '<' => copy(c = c - 1)
    }
  }

  override def part1: Stream[IO, Unit] =
    for {
      grid <- Stream.eval(Ref[IO].of(Grid.empty))
      guard <- Stream.eval(Ref[IO].of(Guard(0,0,'X')))
      _ <- input
        .evalMap { line =>
          val findGuard = "[\\^>v<]".r.findFirstMatchIn(line).traverse_ { m =>
            grid.get.flatMap(grid => guard.set(Guard(grid.nRows, m.start, m.matched(0))))
          }
          val addRow = grid.update(_.withRow(line.replaceAll("[\\^>v<]", ".")))
          findGuard >> addRow
        }
        .onFinalize {
          (grid.get, guard.get).flatMapN { (grid, guard) =>
            IO.println(grid.rows.mkString("\n")) >>
              IO.println(s"Guard's position is $guard")

            def nextGridPoint(g: Guard): Option[Char] = g.o match {
              case '^' => grid(g.r - 1, g.c)
              case 'v' => grid(g.r + 1, g.c)
              case '>' => grid(g.r, g.c + 1)
              case '<' => grid(g.r, g.c - 1)
            }

            @tailrec
            def patrol(guard: Guard, visited: Set[(Int, Int)]): Set[(Int, Int)] = {
              nextGridPoint(guard) match {
                case None => visited + ((guard.r, guard.c))
                case Some('#') => patrol(guard.turn, visited + ((guard.r, guard.c)))
                case Some(_) => patrol(guard.move, visited + ((guard.r, guard.c)))
              }
            }

            val visited = patrol(guard, Set.empty)
            IO.println(s"Guard has visited ${visited.size} points")
          }
        }
    } yield ()

  override def part2: Stream[IO, Unit] =
    for {
      grid <- Stream.eval(Ref[IO].of(Grid.empty))
      guard <- Stream.eval(Ref[IO].of(Guard(0,0,'X')))
      _ <- input
        .evalMap { line =>
          val findGuard = "[\\^>v<]".r.findFirstMatchIn(line).traverse_ { m =>
            grid.get.flatMap(grid => guard.set(Guard(grid.nRows, m.start, m.matched(0))))
          }
          val addRow = grid.update(_.withRow(line.replaceAll("[\\^>v<]", ".")))
          findGuard >> addRow
        }
        .onFinalize {
          (grid.get, guard.get).flatMapN { (grid, guard) =>
            IO.println(grid.rows.mkString("\n")) >>
              IO.println(s"Guard's position is $guard")

            def nextGridPoint(grid: Grid, g: Guard): Option[Char] = g.o match {
              case '^' => grid(g.r - 1, g.c)
              case 'v' => grid(g.r + 1, g.c)
              case '>' => grid(g.r, g.c + 1)
              case '<' => grid(g.r, g.c - 1)
            }

            @tailrec
            def patrol(grid: Grid, guard: Guard, visited: Set[(Int, Int)]): Set[(Int, Int)] = {
              nextGridPoint(grid, guard) match {
                case None => visited + ((guard.r, guard.c))
                case Some('#') => patrol(grid, guard.turn, visited + ((guard.r, guard.c)))
                case Some(_) => patrol(grid, guard.move, visited + ((guard.r, guard.c)))
              }
            }

            val visited = patrol(grid, guard, Set.empty)

            def nextGuardPosition(grid: Grid, guard: Guard): Option[Guard] =
              nextGridPoint(grid, guard).map {
                case '#' => guard.turn
                case _ => guard.move
              }

            @tailrec
            def patrolLoops(grid: Grid, guard: Guard, positions: Set[Guard]): Boolean = {
              nextGuardPosition(grid, guard) match {
                case None => false
                case Some(guard) => if positions(guard) then true else patrolLoops(grid, guard, positions + guard)
              }
            }

            val loops = visited.filter { point =>
              patrolLoops(grid.replaced(point._1, point._2, '#'), guard, Set.empty)
            }

            IO.println(s"Guard has ${loops.size} loops")
          }
        }
    } yield ()

}
