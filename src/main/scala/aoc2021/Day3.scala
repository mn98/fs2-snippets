package aoc2021

import cats.effect.{IO, IOApp, Ref}
import cats.syntax.all.*
import fs2.io.file.{Files, Flags, Path}
import fs2.{Stream, text}

object Day3 extends IOApp.Simple {

  def input: fs2.Stream[IO, String] =
    Files[IO]
      .readAll(Path(getClass.getResource("/aoc/AOC_3_input.txt").getPath), 1024, Flags.Read)
      .through(text.utf8.decode andThen text.lines)
      .filterNot(_.isEmpty)

  def lifeSupportRating(oxygenGeneratorRating: Long, co2ScrubberRating: Long): Long =
    oxygenGeneratorRating * co2ScrubberRating

  def findBitSets(
                   bitSets: Stream[IO, String],
                   bitIndex: Int,
                   findMostCommon: Boolean,
                   favoured: Char): Stream[IO, String] = {
    Stream.eval(Ref[IO].of(0)).flatMap { count =>
      val s1 = bitSets.evalMap { bitSet =>
        val increment = if (bitSet(bitIndex) == '0') -1 else 1
        count.update(_ + increment)
      }
      val s2 = Stream.eval(count.get).flatMap { count =>
        val commonBit = {
          if (count > 0) {
            if (findMostCommon) '1' else '0'
          }
          else if (count < 0) {
            if (findMostCommon) '0' else '1'
          }
          else favoured
        }
        bitSets.filter(bitSet => bitSet(bitIndex) == commonBit)
      }
      s1 >> s2
    }
  }

  def findBitSets2(
                    bitSets: Vector[String],
                    bitIndex: Int,
                    findMostCommon: Boolean,
                    favoured: Char): Vector[String] = {
    if (bitSets.size > 1) {
      var count = 0
      bitSets.foreach { bitSet =>
        val increment = if (bitSet(bitIndex) == '0') -1 else 1
        count += increment
      }
      val commonBit = {
        if (count > 0) {
          if (findMostCommon) '1' else '0'
        }
        else if (count < 0) {
          if (findMostCommon) '0' else '1'
        }
        else favoured
      }
      bitSets.filter(bitSet => bitSet(bitIndex) == commonBit)
    } else {
      bitSets
    }
  }

  val program1: Stream[IO, Unit] = {
    Stream.eval(Ref[IO].of(Map.empty[Int, Int])).flatMap { bits =>
      input.evalMap { b =>
          b.reverse.zipWithIndex.map {
              case (c, i) =>
                val ci = if (c == '0') -1 else 1
                bits.update(bits => bits.updated(i, bits.getOrElse(i, 0) + ci))
            }
            .toList.sequence.void
        }
        .onFinalize(
          bits.get.flatMap { bits =>
            val gamma = bits.keys.toSeq.sorted.foldLeft("") { case (s, c) =>
              val common = if (bits(c) > 0) 1 else 0
              s"$common$s"
            }
            val epsilon = gamma.map(c => if (c == '0') '1' else '0')
            val gammaD = BigInt.apply(gamma, 2)
            val epsilonD = BigInt.apply(epsilon, 2)
            val power = gammaD * epsilonD
            IO(println(s"gamma is $gamma, $gammaD, epsilon is $epsilon, $epsilonD, power is $power"))
          }
        )
    }
  }

  val program2: Stream[IO, Unit] = {

    Stream.eval(Ref[IO].of(0)).flatMap { o2gr =>
      Stream.eval(Ref[IO].of(0)).flatMap { co2sr =>

        val o2p = Stream.eval(Ref[IO].of(input.compile.toVector)).flatMap { bitSets =>
          Stream.emits(0 to 11).evalMap { i =>
            IO(println(s"Processing bit $i")) >>
              bitSets.update(bitSets => bitSets.map(findBitSets2(_, i, findMostCommon = true, '1')))
          }.onFinalize {
            bitSets.get.flatMap { bitSets =>
              bitSets.flatMap { v =>
                val d = Integer.parseInt(v.head, 2)
                IO(println(s"The vector: $v, o2gr $d")) >> o2gr.set(d)
              }
            }
          }
        }

        val co2p = Stream.eval(Ref[IO].of(input.compile.toVector)).flatMap { bitSets =>
          Stream.emits(0 to 11).evalMap { i =>
            IO(println(s"Processing bit $i")) >>
              bitSets.update(bitSets => bitSets.map(findBitSets2(_, i, findMostCommon = false, '0')))
          }.onFinalize {
            bitSets.get.flatMap { bitSets =>
              bitSets.flatMap { v =>
                val d = Integer.parseInt(v.head, 2)
                IO(println(s"The vector: $v, co2sr $d")) >> co2sr.set(d)
              }
            }
          }
        }

        Stream(o2p, co2p).parJoinUnbounded.onFinalize {
          o2gr.get.flatMap { o2gr =>
            co2sr.get.flatMap { co2sr =>
              val lsr = lifeSupportRating(o2gr, co2sr)
              IO(println(s"Life support rating is $lsr"))
            }
          }
        }

      }
    }
  }

  override def run: IO[Unit] = (program1 ++ program2).compile.drain
}
