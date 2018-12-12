package v0

/**
  * Main.scala
  */
object Main extends App {
  import scala.concurrent.Future
  import akka.http.scaladsl.Http
  import Globals._
  import Implicits._
  val app = for {
    binding <- Http().bindAndHandle(
      SignupRoute.route,
      "localhost",
      8080
    )
    _ = println(s"Server up\nPress RETURN to stop")
    _ = stdin.readLine()
    _ <- Future(binding.unbind())
    _ <- Future(system.terminate())
  } yield {
    ()
  }
}
