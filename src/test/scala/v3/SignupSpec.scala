package v3

import cats.Id
import cats.~>
import cats.syntax.either._
import cats.data.WriterT
import cats.instances.all._
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

  "signup" should "return logs when ok" in {
    val f = Fixtures() // Defaults as before.
    val (l, ok) = signup(r).foldMap(compiler(f))
    l shouldBe(Vector(
      LogEntry(INFO, LogMessage("Parsed body")),
      LogEntry(INFO, LogMessage("Created salt")),
      LogEntry(INFO, LogMessage("Created hash")),
      LogEntry(INFO, LogMessage("Persisted user"))
    ))
    ok shouldBe(OK)
  }

  it should "return logs when parse fails" in {
    val f = Fixtures(request = ParseError.asLeft)
    val (l, err) = signup(r).foldMap(compiler(f))
    l shouldBe(Vector(
      LogEntry(ERROR, LogMessage("Parse body failed"))
    ))
    err shouldBe(ClientErr)
  }

  it should "return logs when salt fails" in {
    val f = Fixtures(salt = CryptoError.asLeft)
    val (l, err) = signup(r).foldMap(compiler(f))
    l shouldBe(Vector(
      LogEntry(INFO, LogMessage("Parsed body")),
      LogEntry(ERROR, LogMessage("Create salt failed"))
    ))
    err shouldBe(ServerErr)
  }

  type ArgsLog[A] = WriterT[Id, Vector[SignupA[_]], A]
  type AL[A] = ArgsLog[A]

  object ArgsLog {
    def apply[A](s: SignupA[_], a: A): ArgsLog[A] = {
      WriterT[Id, Vector[SignupA[_]], A]((Vector(s), a))
    }
    def lift[A](s: (SignupA[_], A)): ArgsLog[A] = {
      val (c, a) = s
      apply(c, a)
    }
  }

  def accCompiler[A: IsParseable[Id, ?]]
    (f: Fixtures): SignupA ~> ArgsLog =
    new (SignupA ~> ArgsLog) {
      def apply[C](fa: SignupA[C]): AL[C] = fa match {
        case p: Parse[Id, A] =>
          (p, f.request)     |> ArgsLog.lift
        case g: GenSalt      =>
          (g, f.salt)        |> ArgsLog.lift
        case g: GenHash      =>
          (g, f.hash)        |> ArgsLog.lift
        case p: Persist      =>
          (p, f.result)      |> ArgsLog.lift
      }
    }

  it should "pass correct args" in {
    val f = Fixtures() // Defaults as before.
    val (ss, _) = signup(r).foldMap(accCompiler(f)).run
    ss shouldBe(Vector(
      Parse(r),
      GenSalt(512),
      GenHash(p, s, 10000),
      Persist(User(e, h, s))
    ))
  }
}
