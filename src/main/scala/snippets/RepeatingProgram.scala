package snippets

import cats.effect.{IO, IOApp}
import fs2.Stream

import scala.concurrent.duration.DurationInt

object RepeatingProgram extends IOApp.Simple {

  def config: IO[String] = IO("Some config")

  override def run: IO[Unit] = {
    config.flatMap { config =>
      val program =
        Stream.eval(IO(println(s"Running with configuration: $config"))) ++
          Stream
            .awakeEvery[IO](1.second)
            .evalMap(age => IO(println(s"Time elapsed is $age")))
            .interruptAfter(10.seconds)

      program.compile.drain
    }
  }

}