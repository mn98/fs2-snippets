package snippets

import cats.effect.{IO, IOApp, Ref}
import fs2.{Pipe, Stream}

object Broadcasting extends IOApp.Simple {

  val source: Stream[IO, Int] = Stream.emits(1 to 10)

  val program1: Stream[IO, Int] =
    Stream.eval(Ref.of[IO, Int](0)).flatMap { count =>
      Stream.eval(Ref.of[IO, List[Int]](Nil)).flatMap { collection =>

        val counter: Pipe[IO, Int, Int] = _.evalTap(_ => count.update(_ + 1))
        val collector: Pipe[IO, Int, Int] = _.evalTap(i => collection.update(_ :+ i))

        Stream.exec(IO.println("Running program 1")) ++ source
          .broadcastThrough(
            counter,
            collector
          )
          .onFinalize {
            count.get.flatMap(count => IO.println(s"Counted $count elements")) >>
              collection.get.flatMap(collection => IO.println(s"Collected these elements: $collection"))
          }
      }
    }

  val program2: Stream[IO, Any] = {

    val counter: Pipe[IO, Int, Int] =
      _
        .scan(0)((count, _) => count + 1)
        .debug(count => s"Counted $count elements")

    val collector: Pipe[IO, Int, List[Int]] =
      _
        .scan(List.empty[Int])((collection, i) => collection :+ i)
        .debug(collection => s"Collected these elements: $collection")

    Stream.exec(IO.println("Running program 2")) ++ source
      .broadcastThrough(
        counter,
        collector
      )
  }

  override def run: IO[Unit] = (program1 ++ program2).compile.drain

}
