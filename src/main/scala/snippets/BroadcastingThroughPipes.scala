package snippets

import cats.effect.kernel.Ref
import cats.effect.std.{Queue, Random}
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import fs2.{Pipe, Stream}

import scala.concurrent.duration.DurationInt

object BroadcastingThroughPipes extends IOApp {

  private def pipeA(
             state: Ref[IO, Boolean],
             changes: Queue[IO, Long]
           ): Pipe[IO, (Int, Long), Unit] = {
    in =>
      in
        .chunkN(2, allowFewer = false)
        .evalMap { e =>
          val (v, i) = e.last.get
          (state.set(true) >> changes.offer(i)).whenA(v > 50) >>
            IO(println(s"A: ${e.last}"))
        }
  }

  private def pipeB(
             state: Ref[IO, Boolean]
           ): Pipe[IO, (Int, Long), Unit] = {
    in =>
      in.evalMap { e =>
        val (v, _) = e
        IO.sleep(1.second) >>
          IO(println(s"B: $e")) >>
          state.get.flatMap(b => IO(println(s"State: $b"))) >>
          state.set(false).whenA(v > 90)
      }
  }

  val program: Stream[IO, Unit] =
    for {
      rng <- Stream.eval(Random.scalaUtilRandomSeedLong[IO](19791205))
      state <- Stream.eval(Ref.of[IO, Boolean](false))
      changes <- Stream.eval(Queue.unbounded[IO, Long])
      _ <-
        Stream
          .eval(rng.betweenInt(0, 100))
          .repeatN(10)
          .zipWithIndex
          .broadcastThrough(
            pipeA(state, changes),
            pipeB(state)
          )
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    program.compile.drain.as(ExitCode.Success)

}
