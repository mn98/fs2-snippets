package snippets

import cats.effect.{IO, IOApp, Ref}
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.SignallingRef

import scala.concurrent.duration.DurationInt

object ReactionOnChange extends IOApp.Simple {

  val input: Stream[IO, Int] = Stream.emits(0 to 10)

  override def run: IO[Unit] =
    Stream.eval(SignallingRef[IO].of(0)).flatMap { a =>
      val creation: Stream[IO, Int] = input.evalMap(i => a.set(i / 2) *> a.get).metered(1.second)
      val reaction: Stream[IO, Int] = a.changes.discrete

      Stream(
        creation.debug(i => s"Input set to $i"),
        reaction.debug(i => s"Output changed to $i")
      )
        .parJoinUnbounded
    }
      .interruptAfter(10.seconds)
      .compile
      .drain
}
