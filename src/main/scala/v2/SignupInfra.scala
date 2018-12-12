package v2

object SignupInfra {
  import akka.http.scaladsl.model._
  import cats.Id
  import SignupModel._

  implicit object HttpRequestIsParseable
    extends IsParseable[Id, HttpRequest] {

    /**
      * Cheat and use headers here for now. Use IO and
      * circe on Source[ByteString] later.
      */
    def parse(
      a: HttpRequest
    ): ParseError Or SignupRequest = for {
      e <- errorOrHeader(a, "x-email")
      p <- errorOrHeader(a, "x-password")
    } yield {
      SignupRequest(Email(e), Password(p))
    }

    def errorOrHeader(
      a: HttpRequest, n: String
    ): ParseError Or String = a.headers
      .find(h => h.is(n))
      .map(h => Right(h.value))
      .getOrElse(Left(ParseError))
  }

  implicit object HttpResponseHasCodes
    extends HasCodes[HttpResponse] {
    def success: HttpResponse =
      HttpResponse(StatusCodes.OK)
    def clientError: HttpResponse =
      HttpResponse(StatusCodes.BadRequest)
    def serverError: HttpResponse =
      HttpResponse(StatusCodes.InternalServerError)
  }
}
