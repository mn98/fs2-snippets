package snippets

import cats.effect.{IO, IOApp, Ref}
import fs2.{Pipe, Stream}

object Broadcasting extends IOApp.Simple {

  val source: Stream[IO, Int] = Stream.emits(1 to 10)

  val program: Stream[IO, Int] =
    Stream.eval(Ref.of[IO, Int](0)).flatMap { count =>
      Stream.eval(Ref.of[IO, List[Int]](Nil)).flatMap { collection =>

        val counter: Pipe[IO, Int, Int] = _.evalTap(_ => count.update(_ + 1))
        val collector: Pipe[IO, Int, Int] = _.evalTap(i => collection.update(_ :+ i))

        source
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

  override def run: IO[Unit] = program.compile.drain

}
