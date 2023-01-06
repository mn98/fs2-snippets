package snippets

import cats.effect.kernel.Ref
import cats.effect.std.Semaphore
import cats.effect.{Concurrent, IO, IOApp}
import cats.syntax.all.*

import scala.concurrent.duration.DurationInt

object ControlledAccess extends IOApp.Simple {

  trait Controlled[F[_], T[_[_], _], A]:
    def access[B](f: T[F, A] => F[B]): F[B]

  object Controlled:
    def apply[F[_] : Concurrent, T[_[_], _], A](
                                                 a: F[T[F, A]],
                                                 n: Int
                                               ): F[Controlled[F, T, A]] =
      Semaphore[F](n).flatMap { semaphore =>
        a.map { ar =>
          new Controlled[F, T, A] {
            override def access[B](f: T[F, A] => F[B]): F[B] = semaphore.permit.surround(f(ar))
          }
        }
      }

    def ref[F[_] : Concurrent, A](a: A, n: Int): F[Controlled[F, Ref, A]] = Controlled(Ref[F].of(a), n)

    def atomicRef[F[_] : Concurrent, A](a: A): F[Controlled[F, Ref, A]] = ref(a, 1)

  private def parallelIncrements(c: Controlled[IO, Ref, Int], n: Int): IO[Unit] =
    List.fill(n)(c.access(_.updateAndGet(_ + 1).debug() >> IO.sleep(1.second))).parSequence_

  override def run: IO[Unit] =
    Controlled.atomicRef[IO, Int](0).flatMap(parallelIncrements(_, 10)) >>
      Controlled.ref[IO, Int](0, 4).flatMap(parallelIncrements(_, 20))
}
