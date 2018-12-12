package v1

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

  case object Success
  type Success = Success.type

  trait IsParseable[F[_], A] {
    def parse(a: A): F[SignupRequest]
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
    extends SignupA[SignupRequest]
  case class GenSalt(length: Int)
    extends SignupA[Salt]
  case class GenHash(p: Password, s: Salt, i: Int)
    extends SignupA[Hash]
  case class Persist(user: User)
    extends SignupA[Success]

  import cats.free.Free

  type Signup[A] = Free[SignupA, A]

  import cats.free.Free.liftF

  def liftS[A](s: SignupA[A]): Signup[A] =
    liftF[SignupA, A](s)

  def parse[F[_], A: IsParseable[F, ?]](a: A):
    Signup[SignupRequest] = liftS(Parse[F, A](a))

  def salt(length: Int):
    Signup[Salt] = liftS(GenSalt(length))

  def hash(p: Password, s: Salt, i: Int):
    Signup[Hash] = liftS(GenHash(p, s, i))

  def persist(user: User):
    Signup[Success] = liftS(Persist(user))

}
