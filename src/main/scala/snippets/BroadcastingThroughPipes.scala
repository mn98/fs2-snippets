package snippets

import cats.effect.std.Queue
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.concurrent.SignallingRef
import fs2.{Pipe, Stream}

import scala.concurrent.duration.DurationInt
import scala.util.Random

object BroadcastingThroughPipes extends IOApp {

  def pipeA(
             signal: SignallingRef[IO, Boolean],
             changes: Queue[IO, Int]
           ): Pipe[IO, (Int, Int), Unit] = {
    in =>
      in
        .chunkN(2, allowFewer = false)
        .evalMap { e =>
          val (i, v) = e.last.get
          (signal.set(true) >> changes.offer(i)).whenA(v > 50) >>
            IO(println(s"A: ${e.last}"))
        }
  }

  def pipeB(
             signal: SignallingRef[IO, Boolean]
           ): Pipe[IO, (Int, Int), Unit] = {
    in =>
      in.evalMap { e =>
        val (_, v) = e
        IO.sleep(1.second) >>
          IO(println(s"B: $e")) >>
          signal.get.flatMap(b => IO(println(s"Signal: $b"))) >>
          signal.set(false).whenA(v > 90)
      }
  }

  val program: Stream[IO, Unit] =
    for {
      rng <- Stream.eval(IO(new Random(19791205)))
      signal <- Stream.eval(SignallingRef[IO, Boolean](false))
      changes <- Stream.eval(Queue.unbounded[IO, Int])
      _ <-
        Stream
          .range(0, 10)
          .covary[IO]
          .map(i => i -> rng.nextInt(100))
          .broadcastThrough(
            pipeA(signal, changes),
            pipeB(signal)
          )
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    program.compile.drain.as(ExitCode.Success)

}
