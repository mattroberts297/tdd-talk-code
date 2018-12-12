package v1

/**
  * Type-Driven Development in Practice
  *
  * v1 -> Free. No IO. No WriterT. No ReaderT.
  *
  * @author Matt Roberts
  */



// Type-driven development is a style of programming in
// which we write types first and use those types to
// guide the definition of functions. The overall
// process is to write the necessary data types, and
// then, for each function, do the following:
//  1 Write the input and output types.
//  2 Define the function, using the structure of the
//    input types to guide the implementation.
//  3 Refine and edit the type and function definition
//    as necessary. (Brady, 2017)
object SignupRoute {
  import akka.http.scaladsl.server.Directives._
  import cats.{Id, ~>}
  import SignupModel._, SignupImpl._, SignupInfra._

  val route = (post & path("signup")) {
    extractRequest { request =>
      complete {
        signup(request).foldMap(compiler)
      }
    }
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
