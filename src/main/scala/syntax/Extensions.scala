package syntax

import cats.Monad
import cats.syntax.all.*

extension[F[_] : Monad, T] (o: F[Option[T]])

  def orElse(t: F[T]): F[T] =
    o.flatMap {
      case Some(x) => x.pure[F]
      case None => t
    }
