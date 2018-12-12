package v4

/**
  * SignupImpl.scala
  */
object SignupImpl {
  import SignupModel._

  def info(s: String): Log = Vector(LogEntry(INFO, LogMessage(s)))
  def error(s: String): Log = Vector(LogEntry(ERROR, LogMessage(s)))

  import cats.implicits._

  def errorToCode
  [A : HasCodes]
  (error: SignupError): A = error match {
    case ParseError =>
      HasCodes[A].clientError
    case CryptoError =>
      HasCodes[A].serverError
    case UnknownError =>
      HasCodes[A].serverError
    case ConflictError =>
      HasCodes[A].clientError
  }

  def parse[F[_], A: IsParseable[F, ?]](a: A): ParseConfig
    =>  Signup[(Log, ParseError Or SignupRequest)] = c =>
    for {
      eor <- parseS(a, c)
      log =  eor.fold(
        _ => error("Parse body failed"),
        _ => info("Parsed body")
      )
    } yield (log, eor)

  def salt: SaltConfig =>
    Signup[(Log, CryptoError Or Salt)] = c =>
    for {
      eor <- saltS(c)
      log =  eor.fold(
        _ => error("Create salt failed"),
        _ => info("Created salt")
      )
    } yield (log, eor)

  def hash(p: Password, s: Salt): HashConfig =>
    Signup[(Log, CryptoError Or Hash)] = c =>
    for {
      eor <- hashS(p, s, c)
      log =  eor.fold(
        _ => error("Create hash failed"),
        _ => info("Created hash")
      )
    } yield (log, eor)

  def persistW(u: User): PersistConfig =>
    Signup[(Log, SignupError Or Success)] = c =>
    for {
      eor <- persistS(u, c).leftWiden[SignupError]
      log =  eor.fold(
        _ => error("Persist user failed"),
        _ => info("Persisted user")
      )
    } yield (log, eor)

  type Conf = Config

  def signupImpl
  [F[_], A: IsParseable[F, ?], B: HasCodes]
  (a: A)(c: Conf): Signup[(Log, SignupError Or B)] =
    ConfigLogOrT.value {
      for {
        r <- parse(a)        |> ConfigLogOrT.lift
        s <- salt            |> ConfigLogOrT.lift
        h <- hash(r.p, s)    |> ConfigLogOrT.lift
        u =  User(r.e, h, s)
        _ <- persistW(u)     |> ConfigLogOrT.lift
      } yield { HasCodes[B].success }
    } apply { c }

  def signup
  [F[_], A: IsParseable[F, ?], B: HasCodes]
  (a: A)(c: Config):
  Signup[(Log, B)] = LogT.value {
    for {
      eoc <- signupImpl(a)(c) |> LogT.lift
    } yield {
      eoc.left.map(errorToCode[B]).merge
    }
  }
}
