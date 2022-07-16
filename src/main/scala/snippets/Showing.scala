package snippets

import cats.Show
import cats.effect.{IO, IOApp}
import cats.syntax.all._

object Showing extends IOApp.Simple {

  case class Value[T](value: T)

  implicit def showForValue[T]: Show[Value[T]] = (v: Value[T]) => v.value.toString

  case class Item[T](name: String, value: Value[T])

  implicit def showForItem[T: Show]: Show[Item[T]] = (i: Item[T]) => show"<<${i.name}>> has value <<${i.value}>>"

  override def run: IO[Unit] =
    IO.println(show"${Item("A", Value(1.0))}") >>
      IO.println(show"${Some(Item("B", Value("Two")))}")

}
