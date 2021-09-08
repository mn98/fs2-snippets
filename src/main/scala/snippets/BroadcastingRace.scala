package snippets

import cats.effect.kernel.Ref
import cats.effect.{IO, IOApp}
import cats.syntax.all._
import fs2.{Pipe, Pull, Stream}

object BroadcastingRace extends IOApp.Simple {

  type T = Int

  def takeEveryNth[F[_], O](n: Int): Pipe[F, O, O] = in => {
    def go(s: Stream[F, O]): Pull[F, O, Unit] =
      s.pull.drop(n - 1).flatMap {
        case Some(tl) =>
          tl.pull.take(1).flatMap {
            case Some(tl) => go(tl)
            case None => Pull.done
          }
        case None => Pull.done
      }

    go(in).stream
  }

  def source: Stream[IO, T] = Stream.emits(1 to 10000)

  def writes(state: Ref[IO, T]): Pipe[IO, T, T] = _.through(takeEveryNth(42)).evalTap(state.set)

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
        .onFinalize(IO(println("Finished the broadcasting race. Are you confused too?")))
    }
  }

  override def run: IO[Unit] = program.compile.drain

}
