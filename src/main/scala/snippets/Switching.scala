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
      stateOne <- SignallingRef[IO].of(0)
      stateTwo <- SignallingRef[IO].of(0)
      switchState <- SignallingRef.of[IO, Option[Int]](None)
      _ <- {
        def output(i: Int): Stream[IO, Unit] = {
          if (i < 1) stateOne.discrete.evalMap(i => IO(println(s"$i")))
          else stateTwo.discrete.evalMap(i => IO(println(s"$i")))
        }

        Stream(
          odds.metered(1.second).evalMap(stateOne.set),
          evens.metered(1.second).evalMap(stateTwo.set),
          plusOrMinusOne.take(1).evalMap(i => switchState.set(Some(i))) ++
            plusOrMinusOne.drop(1).evalMap(i => switchState.set(Some(i))).metered(5.seconds),
          switchState.discrete.unNone.switchMap(output)
        )
          .parJoinUnbounded
          .interruptAfter(30.seconds)
          .compile
          .drain
      }
    } yield ()

}
