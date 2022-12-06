package aoc2022

import aoc.AOCApp
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.Stream

object Day4 extends AOCApp {

  override def inputFileName: String = "AOC_2022_4.txt"

  case class Section(start: Int, end: Int) {
    def contains(section: Section): Boolean = start <= section.start && end >= section.end

    def overlaps(section: Section): Boolean =
      (section.start >= start && section.start <= end) ||
        (section.end >= start && section.end <= end) ||
        (end >= section.start && end <= section.end) ||
        (start >= section.start && start <= section.end)
  }

  object Section {
    def fromString(s: String): Section = {
      val boundaries = s.split("-").toList
      Section(boundaries.head.toInt, boundaries(1).toInt)
    }

    def oneContainsTheOther(section1: Section, section2: Section): Boolean =
      section1.contains(section2) || section2.contains(section1)
  }

  override def part1: Stream[IO, Unit] =
    countSections((section1, section2) => Section.oneContainsTheOther(section1, section2))

  override def part2: Stream[IO, Unit] =
    countSections((section1, section2) => section1.overlaps(section2))

  private def countSections(test: (Section, Section) => Boolean): Stream[IO, Unit] =
    Stream.eval(Ref[IO].of(0)).flatMap { containedSectionCount =>
      input.map { line =>
        line.split(",").toList match {
          case section1 :: section2 :: Nil => Some(Section.fromString(section1), Section.fromString(section2))
          case _ => None
        }
      }
        .unNone
        .evalMap { (section1, section2) =>
          containedSectionCount.update(_ + 1).whenA(test(section1, section2))
        }
        .onFinalize {
          containedSectionCount.get.flatMap { containedSectionCount =>
            IO.println(s"Number of overlapping sections is $containedSectionCount")
          }
        }
    }


}
