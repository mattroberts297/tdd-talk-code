package v4

import cats.~>
import org.slf4j.LoggerFactory

object SignupInfra {
  import Globals._, Implicits._
  import SignupModel._
  import akka.http.scaladsl.model._
  import akka.http.scaladsl.marshalling.GenericMarshallers
  import akka.http.scaladsl.marshalling.Marshaller
  import akka.stream.scaladsl.Sink
  import cats.effect.IO
  import cats.syntax.either._
  import io.circe._, io.circe.parser._
  import io.circe.generic.extras.semiauto._
  import io.circe.generic.auto._
  import java.security.SecureRandom
  import javax.crypto.SecretKeyFactory
  import javax.crypto.spec.PBEKeySpec

  def config = Config(
    limit = 2048,
    saltLength = 512,
    iterations = 10000,
    keyLength = 512,
    table = 'user,
    key = 'email
  )

  def logger[A](tuple: (Log, A)): A = {
    val (log, a) = tuple
    val sl4fj = LoggerFactory.getLogger("signup")
    log.foreach { e =>
      e.level match {
        case INFO  => sl4fj.info(e.message.value)
        case ERROR => sl4fj.error(e.message.value)
      }
    }
    a
  }

  def compiler[A: IsParseable[IO, ?]]: SignupA ~> IO =
    new (SignupA ~> IO) {
      def apply[B](fa: SignupA[B]): IO[B] = fa match {
        case Parse(a: A, c)   => parse(a, c)
        case GenSalt(c)       => salt(c)
        case GenHash(p, s, c) => hash(p, s, c)
        case Persist(u, c)    => persist(u, c)
      }
    }

  implicit val emailDecoder =
    deriveUnwrappedDecoder[Email]
  implicit val passwordDecoder =
    deriveUnwrappedDecoder[Password]

  implicit class HttpEntityOps(entity: HttpEntity) {
    def utf8String: IO[String] =
      IO.fromFuture(IO(entity.dataBytes
        .reduce(_ ++ _)
        .map(body => body.utf8String)
        .runWith(Sink.head[String])
      ))

    def json[T : Decoder]: IO[ParseError Or T] =
      utf8String
        .map(decode[T])
        .map(_.left.map(_ => ParseError))
  }

  implicit object HttpRequestIsParseable
      extends IsParseable[IO, HttpRequest] {
    def parse(
      a: HttpRequest
    ): IO[ParseError Or SignupRequest] =
      a.entity.json[SignupRequest] handleErrorWith {
        _ => IO.pure(ParseError.asLeft[SignupRequest])
      }
  }

  def parse[F[_], A: IsParseable[F, ?]](
    a: A, c: ParseConfig
  ): F[ParseError Or SignupRequest] = {
    IsParseable[F, A].parse(a)
  }

  implicit class SaltOps(salt: Salt) {
    import java.util.Base64
    def bytes: Array[Byte] =
      Base64.getDecoder.decode(salt.value)
  }

  implicit class ByteArrayOps(bytes: Array[Byte]) {
    import java.util.Base64
    def base64: String =
      Base64.getEncoder.encodeToString(bytes)
  }

  implicit class SecureRandomOps(random: SecureRandom) {
    def nextBytes(n: Int): Array[Byte] = {
      val bytes = new Array[Byte](n)
      random.nextBytes(bytes)
      bytes
    }
  }

  def salt(c: SaltConfig): IO[CryptoError Or Salt] = {
    for {
      _ <- IO.shift(blockingExecutionContext)
      r <- IO(SecureRandom.getInstanceStrong)
      s <- IO(Salt(r.nextBytes(c.saltLength).base64))
      _ <- IO.shift(executionContext)
    } yield {
      s.asRight[CryptoError]
    }
  } handleErrorWith { _ =>
    IO.pure(CryptoError.asLeft[Salt])
  }

  def factory(s: String) = SecretKeyFactory.getInstance(s)

  def spec(c: Array[Char], s: Array[Byte], i: Int, l: Int) = new PBEKeySpec(c, s, i, l)

  def hash(p: Password, s: Salt, hc: HashConfig):
      IO[CryptoError Or Hash] = {
    for {
      _  <- IO.shift(blockingExecutionContext)
      c  =  p.value.toCharArray
      b  =  s.bytes
      f  <- IO(factory("PBKDF2WithHmacSHA512"))
      ks <- IO(spec(c, b, hc.iterations, hc.keyLength))
      k  <- IO(f.generateSecret(ks))
      kb <- IO(k.getEncoded)
      _  <- IO.shift(executionContext)
    } yield {
      Hash(kb.base64).asRight[CryptoError]
    }
  } handleErrorWith { _ =>
    IO.pure(CryptoError.asLeft[Hash])
  }

  case class DatabaseUser(email: String, hash: String, salt: String)

  implicit class UserOps(u: User) {
    def toDatabaseFormat: DatabaseUser = {
      DatabaseUser(u.e.value, u.h.value, u.s.value)
    }
  }

  def persist(u: User, c: PersistConfig): IO[DatabaseError Or Success] = {
    import com.gu.scanamo._, com.gu.scanamo.syntax._
    for {
      table  <- IO(Table[DatabaseUser](c.table.name))
      putOp  =  table
        .given(not(attributeExists(c.key)))
        .put(u.toDatabaseFormat)
      result <- IO.fromFuture(IO(
        ScanamoAsync.exec(client)(putOp)))
    } yield {
      result.fold(
        _ => ConflictError.asLeft[Success],
        _ => Success.asRight[DatabaseError]
      )
    }
  } handleErrorWith { _ =>
    UnknownError.asLeft[Success] |> IO.pure
  }

  implicit object HttpResponseHasCodes
    extends HasCodes[HttpResponse] {
    def success: HttpResponse =
      HttpResponse(StatusCodes.OK)
    def clientError: HttpResponse =
      HttpResponse(StatusCodes.BadRequest)
    def serverError: HttpResponse =
      HttpResponse(StatusCodes.InternalServerError)
  }

  implicit def ioMarshaller[A, B](
    implicit m: Marshaller[A, B]
  ): Marshaller[IO[A], B] = {
    GenericMarshallers.futureMarshaller(m).compose(_.unsafeToFuture())
  }
}
