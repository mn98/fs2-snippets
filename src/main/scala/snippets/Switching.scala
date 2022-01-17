package snippets

import cats.effect.{IO, IOApp}
import fs2.Stream
import fs2.concurrent.SignallingRef

import scala.concurrent.duration.DurationInt

object Switching extends IOApp.Simple {

  def numbers(start: Int, next: Int => Int): Stream[IO, Int] =
    Stream.unfold(start)(i => Some(i -> next(i)))

  val odds: Stream[IO, Int] = numbers(1, _ + 2)

  val evens: Stream[IO, Int] = numbers(2, _ + 2)

  val plusOrMinusOne: Stream[IO, Int] = numbers(1, _ * -1)

  override def run: IO[Unit] =
    for {
      stateOne <- SignallingRef.of[IO, Option[Int]](None)
      stateTwo <- SignallingRef.of[IO, Option[Int]](None)
      _ <- {
        def output(i: Int): Stream[IO, Unit] = {
          val state = if (i < 1) stateOne else stateTwo
          state.discrete.unNone.evalMap(i => IO(println(s"$i")))
        }

        Stream(
          odds.metered(1.second).evalMap(i => stateOne.set(Some(i))),
          evens.metered(1.second).evalMap(i => stateTwo.set(Some(i))),
          plusOrMinusOne.meteredStartImmediately(5.seconds).switchMap(output)
        )
          .parJoinUnbounded
          .interruptAfter(30.seconds)
          .compile
          .drain
      }
    } yield ()

}
