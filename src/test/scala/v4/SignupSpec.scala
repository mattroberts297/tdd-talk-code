package v4

import cats.Id
import cats.data.WriterT
import cats.~>
import cats.syntax.either._
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import SignupImpl._
import SignupModel._

trait BaseSpec extends FlatSpec with Matchers with Instances {
  val e = Email("matt@example.com")
  val p = Password("p@ssw0rd")
  val r = SignupRequest(e, p)
  val s = Salt("ab")
  val h = Hash("cd")

  val c = Config(
    limit = 2048,
    saltLength = 1024,
    iterations = 10000,
    keyLength = 512,
    table = 'Users,
    key = 'email
  )

  type ConfLog[A] = WriterT[Id, Vector[Any], A]
  type CL[A] = ConfLog[A]

  object ConfLog {
    def apply[A](c: Any, a: A): ConfLog[A] = {
      WriterT[Id, Vector[Any], A]((Vector(c), a))
    }
    def lift[A](t: (Any, A)): ConfLog[A] = {
      val (c, a) = t
      apply(c, a)
    }
  }

  case class Fixtures(
    request: ParseError Or SignupRequest = r.asRight,
    salt: CryptoError Or Salt = s.asRight,
    hash: CryptoError Or Hash = h.asRight,
    result: DatabaseError Or Success = Success.asRight)
}

class SignupSpec extends BaseSpec {
  def compiler
      [A: IsParseable[Id, ?]]
      (f: Fixtures): SignupA ~> ConfLog =
    new (SignupA ~> ConfLog) {
      def apply[C](fa: SignupA[C]): CL[C] = fa match {
        case Parse(_: A, c)   =>
          (c, f.request)      |> ConfLog.lift
        case GenSalt(c)       =>
          (c, f.salt)         |> ConfLog.lift
        case GenHash(_, _, c) =>
          (c, f.hash)         |> ConfLog.lift
        case Persist(_, c)    =>
          (c, f.result)       |> ConfLog.lift
      }
    }

  "signup" should "pass conf correctly" in {
    import cats.implicits._
    val f = Fixtures() // Defaults as before.
    val (cl, _) = signup(r)(c).foldMap(compiler(f)).run
    cl.forall(_ == c) shouldBe (true)
  }
}
