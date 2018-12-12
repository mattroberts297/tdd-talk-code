package v0

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
}
