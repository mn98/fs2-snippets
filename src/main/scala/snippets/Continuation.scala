package snippets

import cats.effect.kernel.Ref
import cats.effect.{IO, IOApp}
import fs2.Stream

import scala.concurrent.duration.DurationInt

object Continuation extends IOApp.Simple {

  def increment(count: Ref[IO, Int], amount: Int): Stream[IO, Unit] =
    Stream
      .eval(count.updateAndGet(_ + amount))
      .debug(c => s"Incrementing by $amount, count = $c")
      .repeat
      .metered(500.milliseconds)
      .interruptAfter(5.seconds)
      .drain

  val program: Stream[IO, Unit] =
    Stream.eval(Ref.of[IO, Int](0)).flatMap { count =>
      increment(count, +1) ++ increment(count, -1)
    }

  override def run: IO[Unit] = program.compile.drain
}
