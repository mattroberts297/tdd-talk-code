package v1

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

import scala.io.StdIn

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.GenericMarshallers
import akka.http.scaladsl.marshalling.Marshaller
import akka.stream.ActorMaterializer
import cats.effect.IO

object Globals {
  // For blocking file IO or blocking crypto.
  val cachedThreadPool = Executors.newCachedThreadPool()
  val blockingExecutionContext = ExecutionContext.fromExecutor(cachedThreadPool)
  val stdin = StdIn
}

object Implicits {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
}

object Marshallers {
  implicit def ioMarshaller[A, B](
    implicit m: Marshaller[A, B]
  ): Marshaller[IO[A], B] = {
    GenericMarshallers.futureMarshaller(m).compose(_.unsafeToFuture())
  }
}
