package snippets

import cats.effect.{Concurrent, IO, IOApp, Ref}
import cats.syntax.all._
import fs2.Stream
import fs2.concurrent.SignallingRef

import scala.concurrent.duration.DurationInt

object ConditionalProcessing extends IOApp.Simple {

  implicit class ProcessWhen[F[_], O](source: Stream[F, O]) {
    def processWhen[F2[x] >: F[x] : Concurrent]
    (
      f: O => O,
      processWhenTrue: Ref[F2, Boolean]
    ): Stream[F2, O] =
      source.evalMap { o =>
        processWhenTrue.get.map { processWhenTrue =>
          if (processWhenTrue) f(o) else o
        }
      }
  }

  val program: Stream[IO, AnyVal] =
    Stream.eval(SignallingRef.of[IO, Boolean](true)).flatMap { signal =>

      val data: Stream[IO, Int] = Stream.emits[IO, Int](1 to 100)

      val switch: Stream[IO, Unit] = Stream
        .awakeEvery[IO](1.second)
        .evalMap(t => signal.updateAndGet(!_).flatMap(signal => IO(println(s"switched to $signal after $t"))))

      Stream(
        switch,
        data
          .metered(100.millis)
          .processWhen(_ * 2, signal)
          .debug()
      )
        .parJoinUnbounded
        .interruptAfter(5.seconds)
    }

  override def run: IO[Unit] = program.compile.drain

}
