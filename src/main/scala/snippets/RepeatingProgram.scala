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
          Stream.awakeEvery[IO](2.seconds).evalMap(age => IO(println(s"Time elapsed is $age")))

      program.compile.drain
    }
  }

}