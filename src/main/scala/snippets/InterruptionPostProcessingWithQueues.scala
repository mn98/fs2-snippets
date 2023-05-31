package snippets

import cats.effect.std.{Queue, Random}
import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import fs2.{Pipe, Stream}

import scala.concurrent.duration.DurationInt

object InterruptionPostProcessingWithQueues extends IOApp.Simple {

  def run: IO[Unit] = {

    val f: Pipe[IO, Int, Unit] =
      _.evalMap { x =>
        // when x === 7, want the "After sleep" message to be printed out
        IO.println(s"got $x") >> IO.sleep(50.millis) >> IO.println(s"After sleep $x")
      }

    (
      Queue.unbounded[IO, Option[Int]],
      SignallingRef[IO, Boolean](false),
      Random.scalaUtilRandom[IO]
    ).tupled.flatMap { case (q, signal, rng) =>
      val ints = Stream.repeatEval(rng.betweenInt(0, 10))
      val p1 =
        (ints.metered(100.millis) merge ints.metered(50.millis))
          .evalTap(x => signal.set(true).whenA(x === 7))
          .evalTap(x => q.offer(x.some)) // offering after signal is set to simulate real case
          .interruptWhen(signal)
          .onFinalize(IO.println("end of stream") >> q.offer(None))

      val p2 =
        Stream
          .fromQueueNoneTerminated(q)
          .through(f)
          .onFinalize(IO.println("end of queue elements"))

      Stream(p1, p2)
        .parJoin(2)
        .compile
        .drain
    }
  }
}
