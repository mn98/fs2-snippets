package snippets

import cats.effect.kernel.Ref
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream

object TapCounter extends IOApp {

  private val program = {
    Stream.eval(Ref.of[IO, Int](0)).flatMap { counter =>
      Stream.range(1, 10)
        .evalTap(i => IO(println(s"pulled $i")))
        .evalTap(_ => counter.update(_ + 1))
        .onFinalize(counter.get.flatMap(t => IO(println(s"Counted $t"))))
    }
  }

  override def run(args: List[String]): IO[ExitCode] =
    program.compile.drain.as(ExitCode.Success)

}
