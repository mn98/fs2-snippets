package snippets

import cats.effect.{IO, IOApp}
import cats.syntax.all._
import fs2.{Chunk, Pull, Stream}

object Zipping extends IOApp.Simple {

  /**
   * Zips the given streams into a single stream where each element is a list
   * of elements from the input streams. Maintains the indexing of the input
   * streams. Preserves chunkiness. Terminates when any one of the input
   * streams terminates.
   *
   * @param streams The input streams
   * @tparam F The effect type
   * @tparam A The element type
   * @return A single stream.
   */
  def zipMany[F[_], A](streams: List[Stream[F, A]]): Stream[F, List[A]] = {

    val nStreams = streams.size

    def combine(legs: List[Stream.StepLeg[F, A]]): Pull[F, List[A], Unit] = {
      val chunks = legs.map(_.head)
      val elements = chunks.map(_.size).min
      if (elements > 0) {
        val lists = for (i <- 0 until elements) yield chunks.map(_.apply(i))
        Pull.output(Chunk(lists: _*)) >>
          combine(legs.map(leg => leg.setHead(leg.head.drop(elements))))
      } else {
        legs
          .traverse(leg => if (leg.head.isEmpty) leg.stepLeg else Pull.pure(Some(leg)))
          .map(_.flatten)
          .flatMap { legs =>
            if (legs.size == nStreams) combine(legs) else Pull.done
          }
      }
    }

    streams
      .traverse(_.pull.stepLeg)
      .flatMap(legs => combine(legs.flatten))
      .stream
  }

  def numbers(chunkSize: Int): Stream[IO, Int] =
    Stream.unfoldChunk[IO, Int, Int](0)(i => Some((Chunk(i until i + chunkSize: _*), i + chunkSize)))

  val program: Stream[IO, Unit] = {
    val streams = List(
      numbers(3).take(10),
      numbers(5).take(11),
      numbers(5).take(12)
    )
    zipMany(streams)
      .debug()
      .void
  }

  override def run: IO[Unit] = program.compile.drain

}
