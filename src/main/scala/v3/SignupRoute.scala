package v3

/**
  * Type-Driven Development in Practice
  *
  * v3 -> Free. IO. No WriterT. No ReaderT.
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
  import cats.~>, cats.effect.IO
  import SignupImpl._, SignupInfra._, SignupModel._

  val route = (post & path("signup")) {
    extractRequest { request =>
      complete(signup(request).foldMap(compiler).map(logger))
    }
  }

  def logger[A](tuple: (Log, A)): A = {
    val (log, a) = tuple
    log.foreach(println(_))
    a
  }

  def compiler[A: IsParseable[IO, ?]]: SignupA ~> IO =
    new (SignupA ~> IO) {
      def apply[B](fa: SignupA[B]): IO[B] = fa match {
        case Parse(a: A)      => a.parse
        case GenSalt(l)       => ???
        case GenHash(p, s, i) => ???
        case Persist(user)    => user.persist
      }
    }
}

