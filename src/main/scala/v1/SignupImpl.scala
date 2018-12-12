package v1

/**
  * SignupImpl.scala
  */
object SignupImpl {
  import SignupModel._

  def signup
      [F[_], A: IsParseable[F, ?], B: HasCodes]
      (a: A): Signup[B] =
    for {
      r <- parse(a)
      s <- salt(512)
      h <- hash(r.p, s, 10000)
      u =  User(r.e, h, s)
      _ <- persist(u)
    } yield { HasCodes[B].success }
}
