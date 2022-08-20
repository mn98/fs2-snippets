package snippets

import cats.effect.{IO, IOApp}
import fs2.Stream

object Retrying extends IOApp.Simple {

  val program: Stream[IO, Unit] =
    Stream
      .emits[IO, Int](1 to 3)
      .evalMap(i => IO.println(s"$i")) ++
      Stream.eval(IO.raiseError(new RuntimeException(s"I have failed :-(")))

  def retry(
             s: Stream[IO, Unit],
             numberOfTimes: Int
           ): Stream[IO, Unit] =
    s
      .handleErrorWith { throwable =>
        Stream.exec(IO.println(s"Error encountered: ${throwable.getMessage}")) ++ {
          val remaining = numberOfTimes - 1
          if (remaining > 1)
            Stream.exec(IO.println(s"Retries remaining: $remaining")) ++ retry(s, remaining)
          else
            Stream.exec(IO.println("No more retries allowed."))
        }
      }

  override def run: IO[Unit] =
    retry(program, 10)
      .compile
      .drain

}
