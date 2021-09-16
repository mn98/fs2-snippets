package snippets

import cats.effect.{IO, IOApp}
import fs2.Stream
import fs2.concurrent.SignallingRef

import scala.concurrent.duration.DurationInt

object Pausing extends IOApp.Simple {

  private val program: Stream[IO, Unit] = {
    Stream.eval(SignallingRef.of[IO, Boolean](true)).flatMap { signal =>
      val timer = Stream.awakeEvery[IO](2.seconds).evalMap(d => IO(println(s"$d elapsed")))
      val stuff = Stream.repeatEval(IO(println("I am doing stuff..."))).metered(1.second).pauseWhen(signal)
      val switch = Stream.repeatEval(signal.update(!_) >> IO(println("switched!"))).metered(5.seconds)
      Stream(
        timer,
        stuff,
        switch
      )
        .parJoinUnbounded
        .interruptAfter(30.seconds)
    }
  }

  override def run: IO[Unit] = program.compile.drain
}
