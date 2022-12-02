package aoc2021

import cats.effect.{IO, Ref}
import fs2.Stream

object Dec1 extends AOCApp {

  override def inputFileName: String = "AOC_2022_1.txt"

  override def part1: fs2.Stream[IO, Unit] = calorieCounter(1)

  override def part2: fs2.Stream[IO, Unit] = calorieCounter(3)

  def calorieCounter(numberOfElves: Int): Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(Set.empty[Long])).flatMap { maxCalories =>
      Stream.eval(Ref[IO].of(0L)).flatMap { calories =>
        input.evalMap { line =>
          if (line.isEmpty) {
            calories.get.flatMap { calories =>
              maxCalories.update { maxCalories =>
                if (maxCalories.size < numberOfElves)
                  maxCalories + calories
                else {
                  val smallest = maxCalories.min
                  if (calories > smallest) {
                    (maxCalories - smallest) + calories
                  } else {
                    maxCalories
                  }
                }
              }
            } >> calories.set(0)
          } else {
            calories.update(_ + line.toLong)
          }
        }
          .onFinalize {
            maxCalories.get.flatMap { maxCalories =>
              IO.println(s"Total calories of the top $numberOfElves elves is ${maxCalories.sum}")
            }
          }
      }
    }

}
