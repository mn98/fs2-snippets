package snippets

import cats.effect.{IO, IOApp}
import fs2.Stream

import scala.concurrent.duration.DurationInt

object Retrying extends IOApp.Simple {

  val program: Stream[IO, Unit] =
    Stream
      .emits[IO, Int](1 to 3)
      .evalMap(i => IO.println(s"$i")) ++
      Stream.eval(IO.raiseError(new RuntimeException(s"I have failed :-(")))

  def resurrecting(s: Stream[IO, Unit]): Stream[IO, Unit] =
    s
      .attempt
      .rethrow
      .handleErrorWith { throwable =>
        Stream.exec(
          IO.println(s"Error encountered: ${throwable.getMessage}") >>
            IO.println("Retrying...")
        ) ++
          resurrecting(s)
      }

  override def run: IO[Unit] =
    resurrecting(program)
      .interruptAfter(30.seconds)
      .compile
      .drain

}
