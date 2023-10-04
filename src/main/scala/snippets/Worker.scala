package snippets

import cats.effect.kernel.DeferredSource
import cats.effect.std.{Queue, Semaphore, Supervisor}
import cats.effect.{Concurrent, Deferred, IO, IOApp, Resource}
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.Channel

import scala.concurrent.duration.DurationInt

/**
 * A worker to which tasks can be submitted.
 *
 * @tparam F The effect type.
 */
trait Worker[F[_]]:
  /**
   *
   * @param task A task to perform.
   * @tparam A The task's result type.
   * @return The optional deferred result of the task.
   */
  def submit[A](task: F[A]): F[Option[DeferredSource[F, A]]]

object Worker:

  /**
   * A worker that drops requested tasks if it is already performing `max` tasks.
   *
   * @param max The maximum allowed number of in-progress tasks.
   * @tparam F The effect type.
   * @return A worker.
   */
  def dropping[F[_] : Concurrent](max: Int): Resource[F, Worker[F]] =
    Supervisor(false).evalMap { supervisor =>
      Queue.dropping[F, Unit](max).map { q =>
        new Worker[F] {
          def submit[A](task: F[A]): F[Option[DeferredSource[F, A]]] =
            q.tryOffer(()).flatMap { accepted =>
              if (accepted) {
                Deferred[F, A].flatMap { result =>
                  supervisor.supervise(task.flatMap(result.complete) >> q.take) *> Some(result).pure
                }
              } else {
                None.pure
              }
            }
        }
      }
    }

  /**
   * A worker that enqueues all tasks that are submitted to it.
   *
   * @param max The maximum number of concurrent, in-progress tasks.
   * @tparam F The effect type.
   * @return A worker.
   */
  def queueing[F[_] : Concurrent](max: Int): Resource[F, Worker[F]] =
    Supervisor(false).evalMap { supervisor =>
      Semaphore[F](max).map { s =>
        new Worker[F] {
          def submit[A](task: F[A]): F[Option[DeferredSource[F, A]]] =
            Deferred[F, A].flatMap { result =>
              supervisor.supervise(s.permit.surround(task.flatMap(result.complete))) *> Some(result).pure
            }
        }
      }
    }

  /**
   * A worker that executes all submitted tasks sequentially.
   *
   * @tparam F The effect type.
   * @return A worker.
   */
  def sequential[F[_] : Concurrent]: Resource[F, Worker[F]] =
    Supervisor(false).evalMap { supervisor =>
      Channel.unbounded[F, F[Boolean]].flatMap { channel =>
        val worker: Worker[F] = new Worker[F] {
          def submit[A](task: F[A]): F[Option[DeferredSource[F, A]]] =
            Deferred[F, A].flatMap { result =>
              channel.send(task.flatMap(result.complete)) *> Some(result).pure
            }
        }
        val work = supervisor.supervise(
          channel
            .stream
            .map(Stream.eval)
            .flatten
            .debug(e => s"Channel emitted $e")
            .compile
            .drain
        )
        work *> worker.pure
      }
    }

object WorkerTest extends IOApp.Simple:

  private val numbers: Stream[IO, Int] = Stream.unfold(0)(i => Some(i -> (i + 1)))

  private val tasks = numbers.map { i =>
    IO.println(s"Starting worker $i") >> IO.sleep(5.seconds) >> IO.println(s"Finished worker $i") >> IO.pure(i * 2)
  }

  private def program(worker: Worker[IO]) =
    Supervisor[IO].use { supervisor =>
      tasks
        .zipWithIndex
        .metered(1.second)
        .evalMap((task, i) => IO.println(s"Submitting task $i") >> worker.submit(task).flatMap {
          case Some(result) =>
            IO.println(s"Accepted task $i") >>
              supervisor.supervise(result.get.flatMap(r => IO.println(s"Result of task $i: $r")))
          case None => IO.println(s"Rejected task $i")
        })
        .interruptAfter(21.seconds)
        .compile
        .drain
    }

  override def run: IO[Unit] =
    IO.println("A dropping worker") >> Worker.dropping[IO](3).use(program) >>
      IO.println("A queueing worker") >> Worker.queueing[IO](3).use(program) >>
      IO.println("A sequential worker") >> Worker.sequential[IO].use(program)
