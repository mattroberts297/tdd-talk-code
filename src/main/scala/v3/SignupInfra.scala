package v3

object SignupInfra {
  import Globals._, Implicits._
  import SignupModel._
  import akka.http.scaladsl.model._
  import cats.effect.IO
  import cats.syntax.either._
  import io.circe._, io.circe.parser._
  import io.circe.generic.extras.semiauto._
  import io.circe.generic.auto._

  implicit val emailDecoder =
    deriveUnwrappedDecoder[Email]
  implicit val passwordDecoder =
    deriveUnwrappedDecoder[Password]

  implicit class HttpEntityOps(entity: HttpEntity) {
    import akka.stream.scaladsl.Sink

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
    def parse(a: HttpRequest):
    IO[ParseError Or SignupRequest] = {
      a.entity.json[SignupRequest]
    }
  }

  implicit class ParseableOps[F[_], A: IsParseable[F, ?]](a: A) {
    def parse: F[ParseError Or SignupRequest] =
      IsParseable[F, A].parse(a)
  }

  implicit class PasswordOps(password: Password) {
    import Globals._
    import Implicits._
    import cats.syntax.either._

    import java.security.SecureRandom
    import javax.crypto.SecretKeyFactory
    import javax.crypto.spec.PBEKeySpec

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

    def munge: IO[CryptoError Or (Hash, Salt)] = for {
      _ <- IO.shift(blockingExecutionContext)
      result <- IO {
        val passwordChars = password.value.toCharArray
        val random = SecureRandom.getInstanceStrong
        val saltBytes = random.nextBytes(512)
        val salt = Salt(saltBytes.base64)
        val skf = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA512")
        val spec = new PBEKeySpec(passwordChars, saltBytes, 10000, 512)
        val hashKey = skf.generateSecret(spec)
        val hashBytes = hashKey.getEncoded
        val hash = Hash(hashBytes.base64)
        (hash, salt).asRight[CryptoError]
      } handleErrorWith { _ =>
        CryptoError.asLeft[(Hash, Salt)] |> IO.pure
      }
      _ <- IO.shift(executionContext)
    } yield {
      result
    }
  }

  case class DatabaseUser(email: String, hash: String, salt: String)

  implicit class UserOps(user: User) {
    def toDatabaseFormat: DatabaseUser = {
      import user._
      DatabaseUser(email.value, hash.value, salt.value)
    }

    def persist: IO[DatabaseError Or Success] = {
      import com.gu.scanamo._, com.gu.scanamo.syntax._
      for {
        table  <- IO(Table[DatabaseUser]("user"))
        putOp  =  table
          .given(not(attributeExists('email)))
          .put(user.toDatabaseFormat)
        result <- IO.fromFuture(IO(
          ScanamoAsync.exec(client)(putOp)))
      } yield {
        result.fold(
          _ => DatabaseConflictError.asLeft[Success],
          _ => Success.asRight[DatabaseError]
        )
      }
    } handleErrorWith { _ =>
      UnknownDatabaseError.asLeft[Success] |> IO.pure
    }
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

  import akka.http.scaladsl.marshalling.GenericMarshallers
  import akka.http.scaladsl.marshalling.Marshaller

  implicit def ioMarshaller[A, B](
    implicit m: Marshaller[A, B]
  ): Marshaller[IO[A], B] = {
    GenericMarshallers.futureMarshaller(m).compose(_.unsafeToFuture())
  }
}
