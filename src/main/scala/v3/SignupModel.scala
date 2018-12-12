package v3


/**
  * SignupModel.scala
  */
object SignupModel {
  import cats.free.Free

  implicit class AnyOps[A](val oa: A) extends AnyVal {
    def |>[B](f: A => B) = f(oa)
    def into[B](f: A => B) = f(oa)
  }

  implicit class FreeOrOps[F[_], A, B](
    val free: Free[F, A Or B]
  ) extends AnyVal {
    def leftWiden[AA >: A]: Free[F, Or[AA, B]] =
      for {
        or <- free
      } yield {
        or.left.map[AA](a => a)
      }
  }

  implicit class FreeLogOrOps[F[_], A, B](
    val free: Free[F, (Log, A Or B)]
  ) extends AnyVal {
    def leftWiden[AA >: A]: Free[F, (Log, AA Or B)] =
      for {
        lor <- free
      } yield {
        val (l, or) = lor
        (l, or.left.map[AA](a => a))
      }
  }

  case class SignupRequest(email: Email, password: Password) {
    def e = email
    def p = password
  }
  case class Password(value: String) extends AnyVal
  case class Email(value: String) extends AnyVal
  case class Hash(value: String) extends AnyVal
  case class Salt(value: String) extends AnyVal
  case class HashAndSalt(hash: Hash, salt: Salt)
  case class User(email: Email, hash: Hash, salt: Salt)

  sealed trait SignupError
  case object ParseError extends SignupError
  case object CryptoError extends SignupError
  sealed trait DatabaseError extends SignupError
  case object DatabaseConflictError extends DatabaseError
  case object UnknownDatabaseError extends DatabaseError
  type ParseError = ParseError.type
  type CryptoError = CryptoError.type
  type DatabaseConflictError = DatabaseConflictError.type
  type UnknownDatabaseError = UnknownDatabaseError.type

  case object Success
  type Success = Success.type

  trait IsParseable[F[_], A] {
    def parse(a: A): F[ParseError Or SignupRequest]
  }

  object IsParseable {
    def apply
    [F[_], A : IsParseable[F, ?]]:
    IsParseable[F, A] =
      implicitly[IsParseable[F, A]]
  }

  trait HasCodes[A] {
    def success: A
    def clientError: A
    def serverError: A
  }

  object HasCodes {
    def apply[A : HasCodes]: HasCodes[A] =
      implicitly[HasCodes[A]]
  }

  sealed trait SignupA[A]
  case class Parse[F[_], A: IsParseable[F, ?]](a: A)
    extends SignupA[ParseError Or SignupRequest]
  case class GenSalt(length: Int)
    extends SignupA[CryptoError Or Salt]
  case class GenHash(p: Password, s: Salt, i: Int)
    extends SignupA[CryptoError Or Hash]
  case class Persist(user: User)
    extends SignupA[DatabaseError Or Success]

  type Signup[A] = Free[SignupA, A]

  import cats.free.Free.liftF

  def liftS[A](s: SignupA[A]): Signup[A] =
    liftF[SignupA, A](s)

  def parseS[F[_], A: IsParseable[F, ?]](a: A):
  Signup[ParseError Or SignupRequest] =
    liftS(Parse[F, A](a))

  def saltS(length: Int):
  Signup[CryptoError Or Salt] =
    liftS(GenSalt(length))

  def hashS(p: Password, s: Salt, i: Int):
  Signup[CryptoError Or Hash] =
    liftS(GenHash(p, s, i))

  def persistS(user: User):
  Signup[DatabaseError Or Success] =
    liftS(Persist(user))

  type Or[+A, +B] = Either[A, B]

  object Or {
    import scala.util.Right
    def lift[A, B](value: B): A Or B = Right(value)
  }

  import cats.data.EitherT

  type OrT[F[_], A, B] = EitherT[F, A, B]

  object OrT {
    def apply[F[_], A, B](value: F[A Or B]):
    OrT[F, A, B] = EitherT(value)

    def lift[F[_], A, B](value: F[A Or B]):
    OrT[F, A, B] = apply(value)

    def value[F[_], A, B](t: OrT[F, A, B]):
    F[A Or B] = t.value
  }

  sealed trait LogLevel
  case object INFO extends LogLevel
  case object ERROR extends LogLevel
  case class LogMessage(value: String) extends AnyVal
  case class LogEntry(level: LogLevel, message: LogMessage)

  type Log = Vector[LogEntry]

  def info(s: String): Log = Vector(LogEntry(INFO, LogMessage(s)))
  def error(s: String): Log = Vector(LogEntry(ERROR, LogMessage(s)))

  import cats.data.WriterT

  object LogT {
    def apply[F[_], A, B](value: F[(A, B)]): WriterT[F, A, B] =
      WriterT[F, A, B](value)

    def lift[F[_], A, B](value: F[(A, B)]): WriterT[F, A, B] =
      apply(value)

    def value[F[_], A, B](t: WriterT[F, A, B]): F[(A, B)] =
      t.run
  }

  type LogOrT[F[_], A, B] = EitherT[WriterT[F, Log, ?], A, B]

  object LogOrT {
    def apply[F[_], A, B](value: F[(Log, A Or B)]):
    LogOrT[F, A, B] = EitherT(WriterT(value))

    def lift[F[_], A, B](value: F[(Log, A Or B)]):
    LogOrT[F, A, B] = apply(value)

    def value[F[_], A, B](t: LogOrT[F, A, B]):
    F[(Log, A Or B)] = t.value.run
  }
}
