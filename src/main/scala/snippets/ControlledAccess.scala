package snippets

import cats.effect.kernel.Ref
import cats.effect.std.Semaphore
import cats.effect.{Concurrent, IO, IOApp}
import cats.syntax.all.*

import scala.concurrent.duration.DurationInt

/**
 * A wrapper for evaluating effects of an effectual object of type `T[F, A]` in a controlled fashion.
 * Implementors of `access` determine how that control works.
 *
 * @tparam F The effect type.
 * @tparam T The effectual type.
 * @tparam A The value type contained by the effectual type.
 */
trait Accessor[F[_], T[_[_], _], A]:
  /**
   *
   * @param f An effect on the controlled object.
   * @tparam B The result type from running the supplied effect.
   * @return The result of running the supplied effect in the effect type `F`.
   */
  def access[B](f: T[F, A] => F[B]): F[B]

object Accessor:
  /**
   * Controlled access for concurrent effects.
   * A `Semaphore` is used to limit the maximum allowed concurrency.
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
                                             ): F[Accessor[F, T, A]] =
    Semaphore[F](n).flatMap { semaphore =>
      a.map { ar =>
        new Accessor[F, T, A] {
          override def access[B](f: T[F, A] => F[B]): F[B] = semaphore.permit.surround(f(ar))
        }
      }
    }

  def atomic[F[_] : Concurrent, T[_[_], _], A](a: F[T[F, A]]): F[Accessor[F, T, A]] = Accessor(a, 1)

  def ref[F[_] : Concurrent, A](a: A, n: Int): F[Accessor[F, Ref, A]] = Accessor(Ref[F].of(a), n)

  def atomicRef[F[_] : Concurrent, A](a: A): F[Accessor[F, Ref, A]] = ref(a, 1)

object ControlledAccess extends IOApp.Simple {

  private def debugParallelIncrements(c: Accessor[IO, Ref, Int], n: Int, prefix: String): IO[Unit] =
    List.fill(n)(c.access(_.updateAndGet(_ + 1).debug(prefix) >> IO.sleep(1.second))).parSequence_

  override def run: IO[Unit] =
    Accessor.atomic(Ref[IO].of(0)).flatMap(debugParallelIncrements(_, 5, "Test 1")) >>
      Accessor(Ref[IO].of(0), 4).flatMap(debugParallelIncrements(_, 20, "Test 2")) >>
      Accessor.atomicRef[IO, Int](0).flatMap(debugParallelIncrements(_, 5, "Test 3")) >>
      Accessor.ref[IO, Int](0, 4).flatMap(debugParallelIncrements(_, 20, "Test 4"))
}
