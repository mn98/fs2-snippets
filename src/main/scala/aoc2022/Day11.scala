package aoc2022

import aoc.AOCApp
import cats.effect.IO
import cats.effect.kernel.Ref
import fs2.Stream

import scala.util.matching.Regex

object Day11 extends AOCApp {

  override def inputFileName: String = "AOC_2022_11.txt"

  case class Monkey(
                     items: Seq[BigInt],
                     inspected: Long,
                     operation: BigInt => BigInt,
                     testFactor: Int,
                     whenPassedThrowTo: Int,
                     whenFailedThrowTo: Int,
                   ) {
    def throwItem(item: BigInt): Int = {
      if item % testFactor == 0 then whenPassedThrowTo else whenFailedThrowTo
    }

    def status: String = s"Holding: ${items.mkString(",")}, inspected: $inspected"
  }

  object Monkey {
    def default: Monkey = Monkey(Seq.empty, 0, _ => 0, 0, 0, 0)

    val monkey: Regex = "Monkey ([0-9]+):".r
    val items: Regex = "\\s*Starting items: (.*)".r
    val operation: Regex = "\\s*Operation: new = (.*)".r
    val opAdd: Regex = "old \\+ ([0-9]+)".r
    val opTimes: Regex = "old \\* ([0-9]+)".r
    val opSquared: Regex = "old \\* old".r
    val test: Regex = "\\s*Test: divisible by ([0-9]+)".r
    val whenTrue: Regex = "\\s*If true: throw to monkey ([0-9]+)".r
    val whenFalse: Regex = "\\s*If false: throw to monkey ([0-9]+)".r
  }

  type Monkeys = Ref[IO, Seq[Monkey]]

  private def readMonkeys(monkeys: Monkeys): IO[Unit] =
    Ref[IO].of(Monkey.default).flatMap { monkey =>
      input
        .filter(_.nonEmpty)
        .evalMap {
          case Monkey.monkey(_) =>
            monkey.set(Monkey.default)
          case Monkey.items(items) =>
            monkey.update(m => m.copy(items = items.split(",").map(i => BigInt(i.trim.toInt))))
          case Monkey.operation(op) =>
            monkey.update { m =>
              m.copy(operation = op match {
                case Monkey.opAdd(x) => _ + x.toInt
                case Monkey.opTimes(x) => _ * x.toInt
                case Monkey.opSquared() => x => x * x
              })
            }
          case Monkey.test(x) =>
            monkey.update(m => m.copy(testFactor = x.toInt))
          case Monkey.whenTrue(i) =>
            monkey.update(m => m.copy(whenPassedThrowTo = i.toInt))
          case Monkey.whenFalse(i) =>
            monkey
              .updateAndGet(m => m.copy(whenFailedThrowTo = i.toInt))
              .flatMap(m => monkeys.update(_ :+ m))
          case input =>
            IO.println(s"Unmatched: $input")
        }
        .compile
        .drain
    }

  private def play(
                    monkeys: Monkeys,
                    relieve: BigInt => BigInt
                  ): IO[Unit] = {
    monkeys.update { monkeys =>
      monkeys.indices.foldLeft(monkeys) { case (monkeys, i) =>
        val monkey = monkeys(i)
        monkey.items.foldLeft(monkeys) { (monkeys, item) =>
          val updatedItem = relieve(monkey.operation(item))
          val j = monkey.throwItem(updatedItem)
          val monkey_j = monkeys(j)
          monkeys
            .updated(j, monkey_j.copy(items = monkey_j.items :+ updatedItem))
        }
          .updated(i, monkey.copy(items = Seq.empty, inspected = monkey.inspected + monkey.items.size))
      }
    }
  }

  override def part1: Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(Seq.empty[Monkey])).flatMap { monkeys =>
      Stream.eval(readMonkeys(monkeys)) ++
        Stream
          .eval(play(monkeys, _ / 3))
          .repeatN(20)
          .onFinalize {
            monkeys.get.flatMap { monkeys =>
              val monkeyBiz = monkeys.map(_.inspected).sorted.takeRight(2).product
              IO.println(s"Monkeys\n${monkeys.map(_.status).mkString("\n")}") >>
                IO.println(s"Monkey business is $monkeyBiz")
            }
          }
    }

  override def part2: Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(Seq.empty[Monkey])).flatMap { monkeys =>
      Stream.eval(readMonkeys(monkeys)) ++
        Stream.eval(monkeys.get.map(_.map(_.testFactor.toLong).product)).flatMap { commonDenominator =>
          Stream
            .eval(play(monkeys, _ % commonDenominator))
            .repeatN(10000)
            .onFinalize {
              monkeys.get.flatMap { monkeys =>
                val monkeyBiz = monkeys.map(_.inspected).sorted.takeRight(2).product
                IO.println(s"Monkeys\n${monkeys.map(_.status).mkString("\n")}") >>
                  IO.println(s"Monkey business is $monkeyBiz")
              }
            }
        }
    }


}
