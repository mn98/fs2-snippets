package snippets

import cats.effect.{IO, IOApp}
import fs2.{Pipe, Stream}

import scala.concurrent.duration.DurationInt

object Broadcasting extends IOApp.Simple {

  def source: Stream[IO, Int] = Stream.emits(1 to 10).chunkLimit(1).unchunks

  val multiplier: Pipe[IO, Int, Int] = _.debug(i => s"I multiply $i by two and get ${i * 2.0}")
  val divider: Pipe[IO, Int, Int] = _.debug(i => s"I divide $i by two and get ${i / 2.0}")

  override def run: IO[Unit] =
    source
      .broadcastThrough(
        multiplier,
        divider
      )
      .meteredStartImmediately(1000.millis)
      .compile
      .drain
}
