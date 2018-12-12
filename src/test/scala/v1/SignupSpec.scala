package v1

import cats.Id
import cats.~>
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import SignupModel._
import SignupImpl._

trait BaseSpec extends FlatSpec with Matchers with Instances {
  case class Fixtures(
    request: SignupRequest, salt: Salt, hash: Hash)
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
        case Persist(u)       => Success
      }
    }

  "signup" should "compile and return OK" in {
    val e = Email("matt@example.com")
    val p = Password("p@ssw0rd")
    val r = SignupRequest(e, p)
    val f = Fixtures(r, Salt("ab"), Hash("cd"))
    signup(r).foldMap(compiler(f)) shouldBe(OK)
  }
}
