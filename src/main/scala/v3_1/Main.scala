package v3_1

import akka.http.scaladsl.Http
import cats.effect.IO

object Main extends App {
  import Globals._
  import Implicits._
  val app = for {
    binding <- IO.fromFuture(IO(Http().bindAndHandle(SignupRoute.route, "localhost", 8080)))
    _ = println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    _ <- IO.shift(blockingExecutionContext)
    _ <- IO(stdin.readLine())
    _ <- IO.shift(executionContext)
    _ <- IO.fromFuture(IO(binding.unbind()))
    _ <- IO.fromFuture(IO(system.terminate()))
    _ <- IO(cachedThreadPool.shutdown())
  } yield {
    ()
  }
  app.unsafeRunSync()
}
