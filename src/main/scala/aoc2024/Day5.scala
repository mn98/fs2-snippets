package aoc2024

import aoc.AOCApp
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.Stream

import scala.util.matching.Regex

object Day5 extends AOCApp {
  override def inputFileName: String = "2024_5.txt"

  object P:
    val cmp: Regex = "([0-9]{2})\\|([0-9]{2})".r
    val blank: Regex = "".r
    val pages: Regex = "([0-9]{1,2})".r

  case class PageSets(before: Set[Int], after: Set[Int])
  opaque type PageOrders = Map[Int, PageSets]
  object PageOrders:
    def empty: PageOrders = Map.empty
  extension (po: PageOrders)
    def addLower(page:Int, lower: Int): PageOrders =
      po.updatedWith(page) {
        case Some(ps) => Some(ps.copy(before = ps.before + lower))
        case None => Some(PageSets(Set(lower), Set.empty))
      }
    def addHigher(page: Int, higher: Int): PageOrders =
      po.updatedWith(page) {
        case Some(ps) => Some(ps.copy(after = ps.after + higher))
        case None => Some(PageSets(Set.empty, Set(higher)))
      }
    def correctOrder(one: Int, two: Int): Option[Boolean] =
      po.get(one).map(ps => !ps.before(two) && ps.after(two))
    def sort(pages: List[Int]): List[Int] = {
      pages.sortWith((one, two) => correctOrder(one, two).contains(true))
    }

  override def part1: Stream[IO, Unit] =
    for {
      po <- Stream.eval(Ref[IO].of(PageOrders.empty))
      middlePages <- Stream.eval(Ref[IO].of(List.empty[Int]))
      _ <- input
        .evalMap {
          case P.blank() => IO.unit
          case P.cmp(l, r) => po.update(_.addLower(r.toInt, l.toInt).addHigher(l.toInt, r.toInt))
          case line =>
            val pages = P.pages.findAllIn(line).toList.map(_.toInt)
            po.get.flatMap { po =>
              val correct = pages.sliding(2).forall { pages =>
                po.correctOrder(pages.head, pages.last).contains(true)
              }
              middlePages.update(_.appended(pages(pages.size / 2))).whenA(correct)
            }
        }
        .onFinalize {
          middlePages.get.flatMap { middlePages =>
            IO.println(s"Sum is ${middlePages.sum}")
          }
        }
    } yield ()

  override def part2: Stream[IO, Unit] =
    for {
      po <- Stream.eval(Ref[IO].of(PageOrders.empty))
      middlePages <- Stream.eval(Ref[IO].of(List.empty[Int]))
      _ <- input
        .evalMap {
          case P.blank() => IO.unit
          case P.cmp(l, r) => po.update(_.addLower(r.toInt, l.toInt).addHigher(l.toInt, r.toInt))
          case line =>
            val pages = P.pages.findAllIn(line).toList.map(_.toInt)
            po.get.flatMap { po =>
              val incorrect = pages.sliding(2).exists { pages =>
                po.correctOrder(pages.head, pages.last).contains(false)
              }
              middlePages.update(_.appended(po.sort(pages)(pages.size / 2))).whenA(incorrect)
            }
        }
        .onFinalize {
          middlePages.get.flatMap { middlePages =>
            IO.println(s"Sum is ${middlePages.sum}")
          }
        }
    } yield ()
}
