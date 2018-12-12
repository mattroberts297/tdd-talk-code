package v1

import cats.Id
import v1.SignupModel.HasCodes
import v1.SignupModel.IsParseable
import v1.SignupModel.SignupRequest

trait Instances {

  sealed trait Result
  case object OK extends Result
  case object Error extends Result

  implicit object SignupIsParseable
    extends IsParseable[Id, SignupRequest] {
    def parse(a: SignupRequest): Id[SignupRequest] = a
  }

  implicit object ResultHasCodes
    extends HasCodes[Result] {
    override def success: Result = OK
    override def clientError: Result = Error
    override def serverError: Result = Error
  }
}
