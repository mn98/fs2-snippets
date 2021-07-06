package snippets

import cats.effect.std.Queue
import cats.effect.{IO, IOApp}
import cats.implicits._
import fs2.concurrent.SignallingRef
import fs2.{Pipe, Stream}

import scala.concurrent.duration.DurationInt

object InterruptionPostProcessingWithQueues extends IOApp.Simple {

  def run: IO[Unit] = {
    val s1 = Stream.repeatEval(IO.sleep(100.millis) >> IO(scala.util.Random.nextInt(10)))
    val s2 = Stream.repeatEval(IO.sleep(50.millis) >> IO(scala.util.Random.nextInt(10)))

    val f: Pipe[IO, Int, Unit] =
      _.evalMap { x =>
        // when x === 7, want the "After sleep" message to be printed out
        IO.println(s"got $x") >> IO.sleep(50.millis) >> IO.println(s"After sleep $x")
      }

    (Queue.unbounded[IO, Option[Int]], SignallingRef[IO, Boolean](false)).tupled.flatMap { case (q, signal) =>
      val p1 =
        s1
          .merge(s2)
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
