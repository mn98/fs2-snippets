package snippets

import cats.effect.kernel.Ref
import cats.effect.std.Queue
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.{Pipe, Stream}

import scala.concurrent.duration.DurationInt
import scala.util.Random

object BroadcastingThroughPipes extends IOApp {

  def pipeA(
             state: Ref[IO, Boolean],
             changes: Queue[IO, Int]
           ): Pipe[IO, (Int, Int), Unit] = {
    in =>
      in
        .chunkN(2, allowFewer = false)
        .evalMap { e =>
          val (i, v) = e.last.get
          (state.set(true) >> changes.offer(i)).whenA(v > 50) >>
            IO(println(s"A: ${e.last}"))
        }
  }

  def pipeB(
             state: Ref[IO, Boolean]
           ): Pipe[IO, (Int, Int), Unit] = {
    in =>
      in.evalMap { e =>
        val (_, v) = e
        IO.sleep(1.second) >>
          IO(println(s"B: $e")) >>
          state.get.flatMap(b => IO(println(s"State: $b"))) >>
          state.set(false).whenA(v > 90)
      }
  }

  val program: Stream[IO, Unit] =
    for {
      rng <- Stream.eval(IO(new Random(19791205)))
      state <- Stream.eval(Ref.of[IO, Boolean](false))
      changes <- Stream.eval(Queue.unbounded[IO, Int])
      _ <-
        Stream
          .range(0, 10)
          .covary[IO]
          .map(i => i -> rng.nextInt(100))
          .broadcastThrough(
            pipeA(state, changes),
            pipeB(state)
          )
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    program.compile.drain.as(ExitCode.Success)

}
