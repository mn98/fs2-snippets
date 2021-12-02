package snippets

import cats.effect.{IO, IOApp}
import fs2.Stream

object Monoids extends IOApp.Simple {

  val program: Stream[IO, Unit] =
    Stream((1, 2), (3, 4), (5, 6))
      .foldMonoid
      .evalMap(IO.println)

  override def run: IO[Unit] = program.compile.drain

}
