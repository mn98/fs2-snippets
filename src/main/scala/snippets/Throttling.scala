package snippets

import cats.effect.{IO, IOApp}
import fs2.Stream
import fs2.timeseries.TimeStamped

import scala.concurrent.duration.DurationInt

object Throttling extends IOApp.Simple {

  val timeSeries: Stream[IO, TimeStamped[Int]] = Stream
    .emits[IO, Int](1 to 100)
    .map(i => TimeStamped(i.seconds, i))

  def playback(factor: Double, chunked: Boolean): Stream[IO, TimeStamped[Int]] = {
    val data = if (chunked) {
      timeSeries
    } else {
      timeSeries.chunkLimit(1).unchunks
    }

    Stream.exec(IO.println(s"Playback at speed ${factor}x ${if (chunked) "with" else "without"} chunks")) ++
      data
        .through(TimeStamped.throttle[IO, Int](factor, 100.millis))
        .debug()
        .interruptAfter(10.seconds)
  }

  val program: Stream[IO, TimeStamped[Int]] =
    playback(1, chunked = true) ++
      playback(2, chunked = true) ++
      playback(1, chunked = false) ++
      playback(2, chunked = false)

  override def run: IO[Unit] = program.compile.drain

}
