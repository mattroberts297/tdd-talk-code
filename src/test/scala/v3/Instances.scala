package v3

import cats.Id
import cats.syntax.either._
import SignupModel._

trait Instances {

  sealed trait Result
  case object OK extends Result
  case object ClientErr extends Result
  case object ServerErr extends Result

  implicit object SignupIsParseable
    extends IsParseable[Id, SignupRequest] {
    def parse(a: SignupRequest): Id[ParseError Or SignupRequest] = a.asRight
  }

  implicit object ResultHasCodes
    extends HasCodes[Result] {
    override def success: Result = OK
    override def clientError: Result = ClientErr
    override def serverError: Result = ServerErr
  }
}
