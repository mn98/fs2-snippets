package aoc2021

import aoc.AOCApp
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

    def getTemplate: F[String]

    def getPairCounts: F[Map[String, Long]]

    def getElementCounts: F[Map[Char, Long]]
  }

  object Polymer {
    def apply[F[_]](
                     template: PolymerTemplate,
                     rules: Set[InsertionRule]
                   )(
                     implicit F: Sync[F]
                   ): F[Polymer[F]] = {

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
                  val updateElementCounter = pairCounter.get.flatMap { pairCounter =>
                    elementCounter.update { elementCounter =>
                      rules.toSeq.foldLeft(elementCounter) { case (elementCounter, rule) =>
                        if (pairCounter.contains(rule.pair)) {
                          elementCounter.updated(
                            rule.element,
                            elementCounter.getOrElse(rule.element, 0L) + pairCounter(rule.pair)
                          )
                        } else {
                          elementCounter
                        }
                      }
                    }
                  }
                  val updatePairCounter = pairCounter.update { pairCounter =>
                    val startOfStepPairCounter = pairCounter
                    //println(s"Start of step pair counts:\n${pairCounter.mkString("\n")}")
                    rules
                      .toSeq
                      .filter(rule => pairCounter.exists { case (p, c) => p == rule.pair && c > 0 })
                      .foldLeft(pairCounter) { case (pairCounter, rule) =>
                        val (pair1, pair2) = rule.pairs
                        val rulePairCount = startOfStepPairCounter(rule.pair)
                        //println(s"Actioning $rule: adding ${rulePairCount}x $pair1 and $pair2, removing ${rule.pair}")
                        val incrementsByPair = Seq(
                          pair1 -> rulePairCount,
                          pair2 -> rulePairCount,
                          rule.pair -> -rulePairCount
                        )
                        val updated = incrementsByPair.foldLeft(pairCounter) {
                          case (pairCounter, (pair, increment)) =>
                            pairCounter.updated(pair, pairCounter.getOrElse(pair, 0L) + increment)
                        }
                        //println(s"Updated pair counts:\n${updated.mkString("\n")}")
                        updated
                      }
                  }
                  updateElementCounter >> updatePairCounter
                }
              }

              override def getTemplate: F[String] = F.delay(template.string)

              override def getPairCounts: F[Map[String, Long]] = pairCounter.get

              override def getElementCounts: F[Map[Char, Long]] = elementCounter.get

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
            case _ => IO.unit
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
                  .repeatN(steps)
                  .onFinalize {
                    polymer.getPairCounts.flatMap { pairCounts =>
                      IO(println(s"Pair counts\n${pairCounts.mkString("\n")}"))
                    } >>
                      polymer.getElementCounts.flatMap { elementCounts =>
                        val sortedCounts = elementCounts.toList.sortBy(_._2)
                        val diff = sortedCounts.last._2 - sortedCounts.head._2
                        IO(println(s"Element counts\n${elementCounts.mkString("\n")}")) >>
                          IO(println(s"Difference is $diff"))
                      }
                  }
              }
            }
          }
    } yield ()
  }


  override def part1: Stream[IO, Unit] = build(10)

  override def part2: Stream[IO, Unit] = build(40)

}
