package syntax

import cats.Functor
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.concurrent.{Signal, SignallingRef}

final class CSwitch[F[_] : Functor] private(val s: SignallingRef[F, CSwitch.State]):
  val state: F[CSwitch.State] = s.get
  val isOn: F[Boolean] = state.map(_ == CSwitch.State.On)
  val isOff: F[Boolean] = state.map(_ == CSwitch.State.Off)
  val CSwitchOn: F[Unit] = s.set(CSwitch.State.On)
  val CSwitchOff: F[Unit] = s.set(CSwitch.State.Off)
  val flip: F[Unit] = s.update {
    case CSwitch.State.On => CSwitch.State.Off
    case CSwitch.State.Off => CSwitch.State.On
  }

object CSwitch:
  enum State:
    case On, Off

  def apply[F[_] : Concurrent](state: State): F[CSwitch[F]] =
    SignallingRef(state).map(signal => new CSwitch(signal))

given[F[_] : Functor]: Conversion[CSwitch[F], Signal[F, Boolean]] =
  _.s.map(_ == CSwitch.State.On)