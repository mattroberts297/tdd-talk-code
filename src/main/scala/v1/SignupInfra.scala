package v1

object SignupInfra {
  import akka.http.scaladsl.model._
  import cats.Id, cats.~>
  import SignupModel._

  implicit object HttpRequestIsParseable
    extends IsParseable[Id, HttpRequest] {

    def parse(a: HttpRequest): SignupRequest = ???
  }

  implicit object HttpResponseHasCodes
    extends HasCodes[HttpResponse] {
    def success: HttpResponse = ???
    def clientError: HttpResponse = ???
    def serverError: HttpResponse = ???
  }

  def compiler[A: IsParseable[Id, ?]]: SignupA ~> Id =
    new (SignupA ~> Id) {
      def apply[B](fa: SignupA[B]): Id[B] = fa match {
        case Parse(a: A)      => ???
        case GenSalt(l)       => ???
        case GenHash(p, s, i) => ???
        case Persist(u)       => ???
      }
    }
}
