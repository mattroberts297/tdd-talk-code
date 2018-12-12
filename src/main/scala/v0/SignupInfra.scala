package v0

/**
  * SignupInfra.scala
  */
object SignupInfra {
  import akka.http.scaladsl.model._
  import scala.concurrent.Future
  import SignupModel._, Implicits._

  def signup
      (request: HttpRequest): Future[HttpResponse] =
    for {
      r <- parse(request)
      s <- salt(512)
      h <- hash(r.p, s, 10000)
      u =  User(r.e, h, s)
      _ <- persist(u)
    } yield { HttpResponse(StatusCodes.OK) }

  def parse
      (r: HttpRequest): Future[SignupRequest] = ???
  def salt
      (length: Int): Future[Salt] = ???
  def hash
      (p: Password, s: Salt, i: Int): Future[Hash] = ???
  def persist
      (user: User): Future[Unit] = ???
}
