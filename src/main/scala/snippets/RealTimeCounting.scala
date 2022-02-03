package snippets

import cats.effect.kernel.Ref
import cats.effect.{IO, IOApp}
import fs2.Stream

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object RealTimeCounting extends IOApp.Simple {

  case class Data(count: Int, elapsed: FiniteDuration)

  val program: Stream[IO, Unit] = {
    Stream.eval(Ref[IO].of(Data(0, 0.milliseconds))).flatMap { data =>
      Stream.awakeEvery[IO](1.millisecond)
        .evalMap(t => data.update(data => Data(data.count + 1, t)))
        .interruptAfter(10.seconds)
        .onFinalize(data.get.flatMap(data =>
          IO(println(s"Counted ${data.count} updates.\nTime elapsed is ${data.elapsed}."))
        ))
    }
  }

  override def run: IO[Unit] = program.compile.drain

}
