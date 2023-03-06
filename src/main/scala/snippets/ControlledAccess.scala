package snippets

import cats.effect.kernel.Ref
import cats.effect.std.Semaphore
import cats.effect.{Concurrent, IO, IOApp}
import cats.syntax.all.*

import scala.concurrent.duration.DurationInt

object ControlledAccess extends IOApp.Simple {

  /**
   * A wrapper for evaluating effects of an effectual object of type `T[F, A]` in a controlled fashion.
   * Implementors of `access` determine how that control works.
   *
   * @tparam F The effect type.
   * @tparam T The effectual type.
   * @tparam A The value type contained by the effectual type.
   */
  trait Controlled[F[_], T[_[_], _], A]:
    /**
     *
     * @param f An effect on the controlled object.
     * @tparam B The result type from running the supplied effect.
     * @return The result of running the supplied effect in the effect type `F`.
     */
    def access[B](f: T[F, A] => F[B]): F[B]

  object Controlled:
    /**
     * Controlled access for concurrent effects.
     * A `Supervisor` is used to limit the maximum allowed concurrency.
     *
     * @param a An effect that produces the object to be controlled.
     * @param n The maximum allowed number of concurrent accesses.
     * @tparam F The effect type.
     * @tparam T The effectual type.
     * @tparam A The value type contained by the effectual type.
     * @return A controlled access wrapper for the supplied `T`.
     */
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
