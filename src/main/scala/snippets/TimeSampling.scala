package snippets

import cats.effect.kernel.Ref
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream

import java.time.Instant
import scala.concurrent.duration.DurationInt

object TimeSampling extends IOApp {

  private val producedRate = 5.seconds
  private val consumedRate = 2.seconds

  private val source: Stream[IO, Int] = {
    Stream.emit[IO, Int](0) ++
      Stream.sleep_[IO](producedRate) ++
      Stream.emit(1) ++
      Stream.sleep_[IO](producedRate)
  }.repeat.chunkLimit(1).unchunks

  val referenced: Stream[IO, Unit] = {
    Stream.eval(Ref.of[IO, Option[Int]](None)).flatMap { sample =>
      val writer = source
        .evalTap(i => sample.set(Some(i)))
        .evalMap(i => IO(println(s"Writing: $i")))
      val reader = Stream
        .eval(sample.get).repeat.metered(consumedRate)
        .evalMap(i => IO(println(s"Reading@${Instant.now}: $i")))
      Stream(writer, reader).parJoinUnbounded
    }
  }

  val debounced: Stream[IO, Unit] = {
    source
      .evalTap(i => IO(println(s"Emitted: $i")))
      .debounce(consumedRate)
      .evalMap(i => IO(println(s"Debounced@${Instant.now}: $i")))
  }

  val held: Stream[IO, Unit] = {
    source
      .holdOption
      .flatMap(_.continuous.metered(consumedRate).unNoneTerminate)
      .evalMap(i => IO(println(s"Held@${Instant.now}: $i")))
  }

  private val program = Stream(referenced, debounced, held).parJoinUnbounded.interruptAfter(60.seconds)

  override def run(args: List[String]): IO[ExitCode] =
    program.compile.drain.as(ExitCode.Success)

}
