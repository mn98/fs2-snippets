package snippets

import cats.Eq
import cats.effect.kernel.Ref
import cats.effect.{Deferred, IO, IOApp}
import cats.syntax.all.*
import fs2.Stream

import scala.concurrent.duration.DurationInt

class CancellableTasks[ID: Eq](cancels: Ref[IO, Map[ID, Deferred[IO, Unit]]]) {

  def run(id: ID, task: IO[Unit]): IO[Unit] =
    Deferred[IO, Unit]
      .flatMap(cancelled =>
        cancels.update(_ + (id -> cancelled)) >>
          IO.println(s"Starting task $id") >>
          task.race(cancelled.get).void
      )
      .guarantee(cancels.update(_ - id) >> IO.println(s"Cancelled task $id"))

  def cancel(id: ID): IO[Unit] =
    cancels.get.map(_.get(id)).flatMap(_.traverse_(_.complete(())))
}

object CancellableTasks extends IOApp.Simple {

  def repeat(message: String): IO[Unit] =
    Stream.eval(IO.println(message)).repeat.metered(1.second).compile.drain

  def program: Stream[IO, Unit] = {
    Stream.eval(Ref[IO].of(Map.empty[Int, Deferred[IO, Unit]])).flatMap { map =>
      val taskManager = CancellableTasks(map)
      val t1 = Stream.eval(taskManager.run(1, repeat("hi")))
      val t2 = Stream.eval(taskManager.run(2, repeat("ho")))
      val ct1 = Stream.eval(taskManager.cancel(1)).delayBy(5.seconds)
      val ct2 = Stream.eval(taskManager.cancel(2)).delayBy(10.seconds)
      Stream(t1, t2, ct1, ct2).parJoinUnbounded
    }
  }

  override def run: IO[Unit] = program.compile.drain
}