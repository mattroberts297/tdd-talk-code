package v3

/**
  * SignupImpl.scala
  */
object SignupImpl {
  import SignupModel._

  def signup
      [F[_], A: IsParseable[F, ?], B: HasCodes]
      (a: A): Signup[(Log, B)] = LogT.value {
    for {
      errorOrCode <- signupImpl[F, A, B](a) |> LogT.lift
    } yield {
      errorOrCode.left.map(errorToCode[B]).merge
    }
  }

  def parse
      [F[_], A: IsParseable[F, ?]](a: A):
      Signup[(Log, ParseError Or SignupRequest)] =
    for {
      eor <- parseS(a)
      log =  eor.fold(
        _ => error("Parse body failed"),
        _ => info("Parsed body")
      )
    } yield (log, eor)

  def salt(length: Int): Signup[(Log, CryptoError Or Salt)] =
    for {
      eor <- saltS(length)
      log =  eor.fold(
        _ => error("Create salt failed"),
        _ => info("Created salt")
      )
    } yield (log, eor)

  def hash(p: Password, s: Salt, i: Int): Signup[(Log, CryptoError Or Hash)] =
    for {
      eor <- hashS(p, s, i)
      log =  eor.fold(
        _ => error("Create hash failed"),
        _ => info("Created hash")
      )
    } yield (log, eor)

  def persistW(user: User): Signup[(Log, SignupError Or Success)] =
    for {
      eor <- persistS(user).leftWiden[SignupError]
      log =  eor.fold(
        _ => error("Persist user failed"),
        _ => info("Persisted user")
      )
    } yield (log, eor)

  import cats.implicits._

  def signupImpl
      [F[_], A: IsParseable[F, ?], B: HasCodes]
      (a: A): Signup[(Log, SignupError Or B)] =
    LogOrT.value {
      for {
        r <- parse(a)            |> LogOrT.lift
        s <- salt(512)           |> LogOrT.lift
        h <- hash(r.p, s, 10000) |> LogOrT.lift
        u =  User(r.email, h, s)
        _ <- persistW(u)         |> LogOrT.lift
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
