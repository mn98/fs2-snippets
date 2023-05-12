package snippets

import cats.effect.{IO, IOApp}
import fs2.Stream
import fs2.concurrent.SignallingRef
import syntax.*

import scala.concurrent.duration.DurationInt

object Pausing extends IOApp.Simple {

  private val program: Stream[IO, Unit] = {
    Stream.eval(Switch[IO](Switch.State.On)).flatMap { switch =>
      val timer = Stream.awakeEvery[IO](2.seconds).evalMap(d => IO(println(s"$d elapsed")))
      val stuff = Stream.repeatEval(IO(println("I am doing stuff..."))).metered(1.second).pauseWhen(switch)
      val flips = Stream.repeatEval(switch.flip >> IO(println("Switch flipped!"))).metered(5.seconds)
      Stream(
        timer,
        stuff,
        flips
      )
        .parJoinUnbounded
        .interruptAfter(30.seconds)
    }
  }

  override def run: IO[Unit] = program.compile.drain
}
