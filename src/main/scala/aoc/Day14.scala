package aoc

import cats.effect.kernel.Ref
import cats.effect.{IO, Sync}
import cats.syntax.all._
import fs2.Stream

object Day14 extends AOCApp {

  override def inputFileName: String = "AOC_14_input.txt"

  case class PolymerTemplate(string: String)

  case class InsertionRule(pair: String, element: Char) {
    require(pair.length == 2)
    val result: String = pair.take(1) + element + pair.takeRight(1)
    val pairs: (String, String) = (pair.take(1) + element, element + pair.takeRight(1))
  }

  trait Polymer[F[_]] {
    def step: F[Unit]

    def get: F[String]

    def getElementCounts: F[Map[Char, Long]]
  }

  object Polymer {
    def apply[F[_]](
                     template: PolymerTemplate,
                     rules: Set[InsertionRule]
                   )(
                     implicit F: Sync[F]
                   ): F[Polymer[F]] = {
      Ref[F].of(template.string).flatMap { polymer =>
        Ref[F].of(0L).flatMap { stepCounter =>

          val initialPairs = {
            val templatePairs = template.string.sliding(2).toList
            templatePairs
              .distinct
              .map(pair => pair -> templatePairs.count(_ == pair).toLong).toMap
          }
          val initialElements = {
            val templateElements = template.string.toCharArray
            templateElements
              .distinct
              .map(element => element -> templateElements.count(_ == element).toLong).toMap
          }

          Ref[F].of(initialPairs).flatMap { pairCounter =>
            Ref[F].of(initialElements).map { elementCounter =>
              new Polymer[F] {

                override def step: F[Unit] = {
                  stepCounter.updateAndGet(_ + 1).flatMap { stepCounter =>
                    F.delay(println(s"Performing step $stepCounter"))
                  } >> {
                    val updates = rules.map { rule =>
                      pairCounter.modify { pairCounter =>
                        if (pairCounter.contains(rule.pair)) {
                          val (pair1, pair2) = rule.pairs
                          val updatedPairCounter = pairCounter
                            .updated(pair1, pairCounter.getOrElse(pair1, 0L) + 1)
                            .updated(pair2, pairCounter.getOrElse(pair2, 0L) + 1)
                            .updated(rule.pair, pairCounter(rule.pair) - 1)
                          val newElement = rule.element
                          (updatedPairCounter, Some(newElement))
                        } else {
                          (pairCounter, None)
                        }
                      }
                        .flatMap { newElement =>
                          newElement.traverse_ { newElement =>
                            elementCounter.update { elementCounter =>
                              elementCounter.updated(newElement, elementCounter.getOrElse(newElement, 0L) + 1)
                            }
                          }
                        }
                    }
                    updates.reduce(_ >> _)
                  }
                }

                //                override def step: F[Unit] = {
                //                  stepCounter.updateAndGet(_ + 1).flatMap { stepCounter =>
                //                    F.delay(println(s"Performing step $stepCounter"))
                //                  } >>
                //                    polymer.update { polymer =>
                //                      val head =
                //                        rules
                //                          .find(_.pair == polymer.take(2))
                //                          .map(_.result)
                //                          .getOrElse(polymer.take(2))
                //                      val tail = polymer.drop(1).sliding(2).toList.map { pair =>
                //                        rules
                //                          .find(_.pair == pair)
                //                          .map(_.result.drop(1))
                //                          .getOrElse(pair)
                //                      }
                //                      (head :: tail).mkString("")
                //                    }
                //                }

                override def get: F[String] = polymer.get

                override def getElementCounts: F[Map[Char, Long]] = elementCounter.get

              }
            }
          }
        }
      }
    }
  }

  def read(
            template: Ref[IO, PolymerTemplate],
            rules: Ref[IO, Set[InsertionRule]]
          ): Stream[IO, Unit] = {
    input
      .take(1)
      .evalMap(line => template.update(_.copy(string = line))) ++
      input
        .drop(2)
        .takeWhile(_.nonEmpty)
        .evalMap { line =>
          line.split("->").toList match {
            case pair :: element :: Nil =>
              rules.update(_ + InsertionRule(pair.trim, element.trim.charAt(0)))
          }
        }
  }

  def build(steps: Int): Stream[IO, Unit] = {
    for {
      template <- Stream.eval(Ref[IO].of(PolymerTemplate("")))
      rules <- Stream.eval(Ref[IO].of(Set.empty[InsertionRule]))
      _ <-
        read(template, rules) ++
          Stream.eval(template.get).flatMap { template =>
            Stream.eval(rules.get).flatMap { rules =>
              Stream.eval(Polymer[IO](template, rules)).flatMap { polymer =>
                Stream
                  .eval(polymer.step)
                  //                  .evalTap(_ =>
                  //                    polymer.get.flatMap { string =>
                  //                      IO(println(s"$string"))
                  //                    }
                  //                  )
                  .repeatN(steps)
                  .onFinalize {
                    polymer.getElementCounts.flatMap { elementCounts =>
                      IO(println(s"Counts\n${elementCounts.mkString("\n")}"))wh
                    }
                    //                    polymer.get.flatMap { string =>
                    //                      val elementCounts = string.toCharArray.distinct.map { element =>
                    //                        element -> string.count(_ == element)
                    //                      }.sortBy(_._2)
                    //                      val smallest = elementCounts.head._2
                    //                      val largest = elementCounts.last._2
                    //                      val difference = largest - smallest
                    //                      IO(println(s"Counts\n${elementCounts.mkString("\n")}")) >>
                    //                        IO(println(s"Difference = $difference"))
                    //                    }
                  }
              }
            }
          }
    } yield ()
  }


  override def part1: Stream[IO, Unit] = build(10)

  override def part2: Stream[IO, Unit] = build(1)

}
