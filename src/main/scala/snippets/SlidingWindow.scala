package snippets

import cats.effect.kernel.Ref
import cats.effect.{IO, IOApp}
import fs2._

object SlidingWindow extends IOApp.Simple {

  private val maxEyes = 3

  override def run: IO[Unit] = {
    Stream.eval(Ref.of[IO, Seq[Int]](Seq.empty)).flatMap { eyes =>
      Stream.range[IO, Int](0, 20)
        .evalTap { i =>
          eyes.update { eyes =>
            if (eyes.size < maxEyes)
              eyes :+ i
            else
              eyes.tail :+ i
          } >>
            eyes.get.flatMap { eyes =>
              if (eyes.size < maxEyes) {
                IO(println(s"Not enough eyes at index $i"))
              } else {
                IO(println(s"Eyes at index $i: $eyes"))
              }
            }
        }
    }
      .compile
      .drain
  }

}
