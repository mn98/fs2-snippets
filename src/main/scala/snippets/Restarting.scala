package snippets

import cats.effect.{IO, IOApp}
import fs2.Stream

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object Restarting extends IOApp.Simple {

  val spawner: Stream[IO, FiniteDuration] = Stream.awakeEvery[IO](5.seconds)

  val counter: Stream[IO, Int] = Stream.unfold(0)(i => Some(i -> (i + 1)))

  override def run: IO[Unit] =
    spawner
      .switchMap { _ =>
        Stream(
          counter.metered(500.millis).debug(i => s"1:$i"),
          counter.metered(1.second).debug(i => s"2:$i")
        ).parJoinUnbounded
      }
      .interruptAfter(30.seconds)
      .compile
      .drain

}
