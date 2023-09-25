package syntax

import cats.Functor
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.concurrent.{Signal, SignallingRef}

trait TSwitch[F[_]]:
  def state: F[TSwitch.State]
  def isOn: F[Boolean]
  def isOff: F[Boolean]
  def switchOn: F[Unit]
  def switchOff: F[Unit]
  def flip: F[Unit]

object TSwitch:
  enum State:
    case On, Off

  def apply[F[_] : Concurrent](initial: State): F[TSwitch[F]] =
    SignallingRef(initial == State.On).map { s =>
      new TSwitch[F] {
        override val state: F[TSwitch.State] = s.get.map(if (_) TSwitch.State.On else TSwitch.State.Off)
        override val isOn: F[Boolean] = state.map(_ == TSwitch.State.On)
        override val isOff: F[Boolean] = state.map(_ == TSwitch.State.Off)
        override val switchOn: F[Unit] = s.set(true)
        override val switchOff: F[Unit] = s.set(false)
        override val flip: F[Unit] = s.update(!_)
      }
    }