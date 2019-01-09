package v3_2

import cats.Id
import cats.~>
import cats.syntax.either._
import cats.instances.all._
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import SignupImpl._
import SignupModel._
import cats.data.WriterT

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
    result: DatabaseError Or Success = Success.asRight,
    log: LogError Or Success = Success.asRight)
}

class SignupSpec extends BaseSpec {
  def compiler
  [A: IsParseable[Id, ?]]
  (f: Fixtures): SignupA ~> Id =
    new (SignupA ~> Id) {
      def apply[C](fa: SignupA[C]): Id[C] = fa match {
        case Parse(_: A)      => f.request
        case GenSalt(_)       => f.salt
        case GenHash(_, _, _) => f.hash
        case Persist(_)       => f.result
        case WriteLog(_)      => f.log
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
    signup(r).foldMap(compiler(f)) shouldBe (ServerErr)
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

  // todo unhappy path logging - this will need to be tweaked with monad transformer
  def logInjector[F[_], A: IsParseable[F, ?]]: SignupA ~> Signup =
    new (SignupA ~> Signup) {
      val l = Success.asRight[LogError]
      import SignupModel.{info => infoA}
      def apply[C](fa: SignupA[C]): Signup[C] = fa match {
        case p: Parse[F, A] =>
          for {
            _ <- log(infoA(s"Parsing body: ${p.a}"))
            ps <- liftS(p)
            _ <- log(infoA(s"Parsed body: $ps"))
          } yield ps
        case g: GenSalt =>
          for {
            _ <- log(infoA(s"Generating salt with length: ${g.length}"))
            s <- liftS(g)
            _ <- log(infoA(s"Generated salt: $s"))
          } yield s
        case g: GenHash =>
          for {
            _ <- log(infoA(s"Generating hash with iterations: ${g.i}"))
            h <- liftS(g)
            _ <- log(infoA(s"Generated hash: $h"))
          } yield h
        case pa: Persist =>
          for {
            _ <- log(infoA(s"Persisting user: ${pa.user}"))
            p <- liftS(pa)
            _ <- log(infoA(s"Persisted user: ${pa.user}"))
          } yield p
        case w: WriteLog => liftS(w)
      }
    }

  def accCompiler[A: IsParseable[Id, ?]]
  (f: Fixtures): SignupA ~> ArgsLog =
    new (SignupA ~> ArgsLog) {
      val l = Success.asRight[LogError]
      def apply[C](fa: SignupA[C]): AL[C] = fa match {
        case p: Parse[Id, A] =>
          (p, f.request)     |> ArgsLog.lift
        case g: GenSalt      =>
          (g, f.salt)        |> ArgsLog.lift
        case g: GenHash      =>
          (g, f.hash)        |> ArgsLog.lift
        case p: Persist      =>
          (p, f.result)      |> ArgsLog.lift
        case w: WriteLog     =>
          (w, l)             |> ArgsLog.lift
      }
    }

  it should "pass correct args" in {
    val f = Fixtures() // Defaults as before.
    val (ss, _) = signup(r).foldMap(accCompiler(f)).run
    ss shouldBe(Vector(
      Parse(r),
      GenSalt(512),
      GenHash(p, s, 10000),
      Persist(User(e, h, s)),
    ))
  }

  it should "inject more logs" in {
    val f = Fixtures() // Defaults as before.
    val (ss, _) = signup(r).foldMap(logInjector).foldMap(accCompiler(f)).run
    ss shouldBe(Vector(
      WriteLog(SignupModel.info(s"Parsing body: $r")),
      Parse(r),
      WriteLog(SignupModel.info(s"Parsed body: ${f.request}")),
      WriteLog(SignupModel.info("Generating salt with length: 512")),
      GenSalt(512),
      WriteLog(SignupModel.info(s"Generated salt: ${f.salt}")),
      WriteLog(SignupModel.info("Generating hash with iterations: 10000")),
      GenHash(p, s, 10000),
      WriteLog(SignupModel.info(s"Generated hash: ${f.hash}")),
      WriteLog(SignupModel.info(s"Persisting user: ${User(e, h, s)}")),
      Persist(User(e, h, s)),
      WriteLog(SignupModel.info(s"Persisted user: ${User(e, h, s)}"))
    ))
  }
}
