package snippets

import cats.effect.kernel.Ref
import cats.effect.{IO, IOApp}
import fs2.Stream

import scala.concurrent.duration.DurationInt

object RealTimeCounting extends IOApp.Simple {

  val program: Stream[IO, Unit] = {
    Stream.eval(Ref[IO].of(0)).flatMap { count =>
      Stream.awakeEvery[IO](1.millisecond)
        .evalMap(t => IO(println(s"Time elapsed is $t")) >> count.update(_ + 1))
        .interruptAfter(10.seconds)
        .onFinalize(count.get.flatMap(count => IO(println(s"Final count is $count"))))
    }
  }

  override def run: IO[Unit] = program.compile.drain

}
