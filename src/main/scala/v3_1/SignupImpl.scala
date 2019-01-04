package v3_1

import cats.free.Free
import cats.syntax.either._

/**
  * SignupImpl.scala
  */
object SignupImpl {
  import SignupModel._

  def signup
  [F[_], A: IsParseable[F, ?], B: HasCodes]
  (a: A): Signup[B] = {
    Success.asRight[LogError]
    for {
      errorOrCode <- signupImpl(a)
      _ <- errorOrCode.fold(logError, _ => Free.pure[SignupA, LogError Or Success](Right(Success)))
    } yield {
      errorOrCode.left.map(errorToCode[B]).merge
    }
  }

  def signupImpl
      [F[_], A: IsParseable[F, ?], B: HasCodes]
      (a: A): Signup[SignupError Or B] =
    OrT.value {
      for {
        r <- parse(a)                     |> OrT.lift
        _ <- log(info("Parsed body"))     |> OrT.lift
        s <- salt(512)                    |> OrT.lift
        _ <- log(info("Created salt"))    |> OrT.lift
        h <- hash(r.p, s, 10000)          |> OrT.lift
        _ <- log(info("Created hash"))    |> OrT.lift
        u =  User(r.e, h, s)
        _ <- persist(u)                   |> OrT.lift
        _ <- logW(info("Persisted user")) |> OrT.lift
      } yield { HasCodes[B].success }
    }

  def logError
  (e: SignupError): Signup[LogError Or Success] = e match {
    case ParseError =>
      log(error(""))
    case CryptoError =>
      log(error(""))
    case UnknownDatabaseError =>
      log(error(""))
    case DatabaseConflictError =>
      log(error(""))
    case LogError =>
      log(error(""))
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
    case LogError =>
      HasCodes[A].serverError
  }
}
