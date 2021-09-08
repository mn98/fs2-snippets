package snippets

import cats.effect.std.{Dispatcher, Queue}
import cats.effect.{IO, IOApp}
import fs2.Stream

object QueuePerformance extends IOApp.Simple {

  override def run: IO[Unit] = {
    Dispatcher[IO].use { dispatcher =>
      Queue.synchronous[IO, Int].flatMap { stage1 =>
        Queue.unbounded[IO, Int].flatMap { stage2 =>

          //val source = Stream.emits(1 to 1000).evalMap(stage1.offer)
          val source = Stream.emits[IO, Int](1 to 10000)
            .evalMap(i => IO(dispatcher.unsafeRunAndForget(stage1.offer(i))))

          val sink1 = Stream.fromQueueUnterminated(stage1)
            .evalTap(i => IO(println(s"-> Enqueueing -> $i")))
            .evalMap(stage2.offer)

          val sink2 = Stream.fromQueueUnterminated(stage2)
            .evalMap(i => IO(println(s"<- Dequeueing <- $i")))

          Stream(source, sink1, sink2).parJoinUnbounded.compile.drain
        }
      }
    }
  }

}
