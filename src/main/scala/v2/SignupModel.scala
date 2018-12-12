package v2

/**
  * SignupModel.scala
  */
object SignupModel {
  case class SignupRequest(email: Email, password: Password) {
    def e = email
    def p = password
  }
  case class Password(value: String) extends AnyVal
  case class Email(value: String) extends AnyVal
  case class Hash(value: String) extends AnyVal
  case class Salt(value: String) extends AnyVal
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

  import cats.free.Free

  type Signup[A] = Free[SignupA, A]

  import cats.free.Free.liftF

  def liftS[A](s: SignupA[A]): Signup[A] =
    liftF[SignupA, A](s)

  def parse[F[_], A: IsParseable[F, ?]](a: A):
  Signup[ParseError Or SignupRequest] =
    liftS(Parse[F, A](a))

  def salt(length: Int):
  Signup[CryptoError Or Salt] =
    liftS(GenSalt(length))

  def hash(p: Password, s: Salt, i: Int):
  Signup[CryptoError Or Hash] =
    liftS(GenHash(p, s, i))

  def persist(user: User):
  Signup[DatabaseError Or Success] =
    liftS(Persist(user))

  def persistW(user: User) =
    persist(user).leftWiden[SignupError]

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
}
