package aoc

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all._
import fs2.Stream

object Day8 extends AOCApp {

  override def inputFileName: String = "AOC_8_input.txt"

  case class Entry(signals: Seq[String], outputs: Seq[String]) {
    def signalSets: Seq[Set[Char]] = signals.map(_.toSet)
  }

  def entries: Stream[IO, Entry] =
    input
      .filterNot(_.isEmpty)
      .map { line =>
        line.split("\\|").toList match {
          case signals :: outputs :: Nil =>
            Entry(
              signals.trim.split(" ").toSeq,
              outputs.trim.split(" ").toSeq
            )
        }
      }

  val part1: Stream[IO, Unit] = {
    Stream.eval(Ref[IO].of(0)).flatMap { counter =>
      entries.evalMap { entry =>
        counter.update(_ + entry.outputs.count(o => Set(2, 4, 3, 7)(o.length)))
      }
        .onFinalize {
          counter.get.flatMap { counter =>
            IO(println(s"1, 4, 7 or 8 appear $counter times in the output values."))
          }
        }
    }
  }

  val part2: Stream[IO, Unit] = {
    Stream.eval(Ref[IO].of(0L)).flatMap { sum =>
      entries.evalMap { entry =>

        val one = entry.signalSets.find(_.size == 2).get
        val seven = entry.signalSets.find(_.size == 3).get
        val four = entry.signalSets.find(_.size == 4).get
        val eight = entry.signalSets.find(_.size == 7).get
        val three = entry.signalSets.filter(_.size == 5).find(d => (d -- seven).size == 2).get
        val nine = entry.signalSets.find(_ == three ++ four).get
        val a = seven -- one
        val b = nine -- three
        val e = eight -- nine
        val g = (three -- four) -- seven
        val zero = one ++ a ++ b ++ e ++ g
        val d = eight -- zero
        val six = entry.signalSets.find(s => s.size == 6 && s != nine && s != zero).get
        val five = six -- e
        val c = eight -- six
        val two = a ++ c ++ d ++ e ++ g

        case class Picker(numbers: Seq[Set[Char]]) {
          def pick(number: Set[Char]): String = numbers.indexOf(number).toString
        }

        val picker = Picker(Seq(zero, one, two, three, four, five, six, seven, eight, nine))

        val outputValue = entry.outputs.foldLeft("") { (numberString, output) =>
          s"$numberString${picker.pick(output.toSet)}"
        }.toLong

        sum.update(_ + outputValue)
      }
        .onFinalize {
          sum.get.flatMap { sum =>
            IO(println(s"The sum of the output values is $sum."))
          }
        }
    }
  }

}
