package syntax

import cats.Functor
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.concurrent.{Signal, SignallingRef}

opaque type Switch[F[_]] = SignallingRef[F, Boolean]

object Switch:
  enum State:
    case On, Off

  def apply[F[_] : Concurrent](s: State): F[Switch[F]] = SignallingRef(s == State.On)

extension[F[_] : Functor] (s: Switch[F])
  def state: F[Switch.State] = s.get.map(s => if (s) Switch.State.On else Switch.State.Off)
  def isOn: F[Boolean] = state.map(_ == Switch.State.On)
  def isOff: F[Boolean] = state.map(_ == Switch.State.Off)
  def switchOn: F[Unit] = s.set(true)
  def switchOff: F[Unit] = s.set(false)
  def flip: F[Unit] = s.update(!_)
  implicit def asSignal: Signal[F, Boolean] = s
