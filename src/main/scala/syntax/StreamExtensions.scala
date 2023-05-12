package syntax

import cats.effect.Concurrent
import fs2.concurrent.Signal
import fs2.{Pull, Stream}

/**
 * Extensions for a single `Stream`.
 */
extension[F[_], A] (s: Stream[F, A])

  def inactiveWhen[F2[x] >: F[x] : Concurrent](p: Signal[F2, Boolean]): Stream[F2, A] =
    p.discrete.dropWhile(_ == true).take(1).drain ++ s.pauseWhen(p)

  def takeEveryNth(n: Int): Stream[F, A] =
    def go(s: Stream[F, A]): Pull[F, A, Unit] =
      s.pull.drop(n - 1).flatMap {
        case Some(tl) =>
          tl.pull.take(1).flatMap {
            case Some(tl) => go(tl)
            case None => Pull.done
          }
        case None => Pull.done
      }

    go(s).stream

/**
 * Extensions for a sequence of `Stream`s.
 */
extension[F[_], A] (s: Seq[Stream[F, A]])

  def zipWith(f: (A, A) => A): Stream[F, A] =
    s.size match
      case 0 => Stream.empty
      case 1 => s.head
      case _ => s.tail.foldLeft(s.head)((s1, s2) => (s1 zipWith s2)(f))

/**
 * Extensions for programs that produce a single `Stream`.
 */
extension[F[_], A] (fs: F[Stream[F, A]])

  def stream: Stream[F, A] = Stream.eval(fs).flatten
