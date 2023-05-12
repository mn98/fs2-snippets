package snippets

import cats.effect.kernel.Ref
import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import fs2.{Pipe, Pull, Stream}
import syntax.*

object BroadcastingRace extends IOApp.Simple {

  type T = Int

  def source: Stream[IO, T] = Stream.emits(1 to 10000)

  def writes(state: Ref[IO, T]): Pipe[IO, T, T] = _.takeEveryNth(42).evalTap(state.set)

  def reads(state: Ref[IO, T]): Pipe[IO, T, T] = _.evalTap { t =>
    state.get.flatMap { s =>
      IO(println(s"state $s is greater than current element $t")).whenA(s > t)
    }
  }

  val program: Stream[IO, T] = {
    Stream.eval(Ref.of[IO, T](0)).flatMap { state =>
      source
        .chunkLimit(1)
        .unchunks
        .broadcastThrough(
          writes(state),
          reads(state)
        )
        .onFinalize(IO(println("Finished the broadcasting race.")))
    }
  }

  override def run: IO[Unit] = program.compile.drain

}
