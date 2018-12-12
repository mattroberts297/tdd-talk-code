package v2

/**
  * SignupImpl.scala
  */
object SignupImpl {
  import SignupModel._

  def signup
  [F[_], A: IsParseable[F, ?], B: HasCodes]
  (a: A): Signup[B] = {
    for {
      errorOrCode <- signupImpl(a)
    } yield {
      errorOrCode.left.map(errorToCode[B]).merge
    }
  }

  def signupImpl
      [F[_], A: IsParseable[F, ?], B: HasCodes]
      (a: A): Signup[SignupError Or B] =
    OrT.value {
      for {
        r <- parse(a)            |> OrT.lift
        s <- salt(512)           |> OrT.lift
        h <- hash(r.p, s, 10000) |> OrT.lift
        u =  User(r.e, h, s)
        _ <- persistW(u)         |> OrT.lift
      } yield { HasCodes[B].success }
    }

  def errorToCode
  [A : HasCodes]
  (error: SignupError): A = error match {
    case ParseError =>
      HasCodes[A].clientError
    case CryptoError =>
      HasCodes[A].serverError
    case UnknownDatabaseError =>
      HasCodes[A].serverError
    case DatabaseConflictError =>
      HasCodes[A].clientError
  }
}
