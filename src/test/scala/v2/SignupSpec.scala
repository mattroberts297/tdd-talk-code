package v2

import cats.Id
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

  case class Fixtures(
    request: ParseError Or SignupRequest = r.asRight,
    salt: CryptoError Or Salt = s.asRight,
    hash: CryptoError Or Hash = h.asRight,
    result: DatabaseError Or Success = Success.asRight)
}

class SignupSpec extends BaseSpec {
  def compiler
      [A: IsParseable[Id, ?]]
      (fixtures: Fixtures): SignupA ~> Id =
    new (SignupA ~> Id) {
      def apply[C](fa: SignupA[C]): Id[C] = fa match {
        case Parse(a: A)      => fixtures.request
        case GenSalt(l)       => fixtures.salt
        case GenHash(p, s, i) => fixtures.hash
        case Persist(u)       => fixtures.result
      }
    }

  "signup" should "compile and return OK" in {
    val f = Fixtures() // Defaults as before.
    signup(r).foldMap(compiler(f)) shouldBe(OK)
  }

  it should "return ClientErr when parse errors" in {
    val f = Fixtures(request = ParseError.asLeft)
    signup(r).foldMap(compiler(f)) shouldBe(ClientErr)
  }

  it should "return ServerErr when hash errors" in {
    val f = Fixtures(salt = CryptoError.asLeft)
    signup(r).foldMap(compiler(f)) shouldBe(ServerErr)
  }
}
