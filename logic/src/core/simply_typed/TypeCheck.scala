package core.simply_typed

/**
  * Created by MiloXia.
  */
trait TypeCheck { self: AbstractSyntax =>

  trait Kind
  case object Star extends Kind

  trait Info
  case class HasKind(k: Kind) extends Info // bool :: *
  case class HasType(t: Type) extends Info // y :: bool

  type Context = Map[Name, Info]

  type Result[A] = Either[String, A]

  def throwError[A](msg: String): Result[A] = Left(msg)
  def returnR[A](res: A): Result[A] = Right(res)

  // check kind of type
  def kind(Γ: Context, tpe: Type, k: Kind): Result[Unit] = (tpe, k) match {
    case (TFree(x), Star) => Γ.get(x) match {
      case Some(HasKind(Star)) => returnR(())
      case None => throwError("unknown identifier")
    }
    case (Fun(a, b), Star) => for {
      _ <- kind(Γ, a, Star)
      r <- kind(Γ, b, Star)
    } yield r
  }

  def type0(Γ: Context, t: InferableTerm): Result[Type] = typeInf(0, Γ, t)

  // for inferable terms
  def typeInf(i: Int, Γ: Context, term: InferableTerm): Result[Type] = term match {
    case Ann(e, t) => for {
      _ <- kind(Γ, t, Star)
      _ <- typeChk(i, Γ, e, t)
    } yield t
    case Free(x)   => Γ.get(x) match {
      case Some(HasType(t)) => returnR(t)
      case _ => throwError("unknown identifier")
    }
    case e :@: ev => for {
      _r <- typeInf(i, Γ, e) // get & check abs type
      r <- _r match {
        case Fun(a, b) => for {
          _ <- typeChk(i, Γ, ev, a) // check var type
          } yield b
        case _ => throwError("illegal application")
      }
    } yield r
  }

  def typeChk(i: Int, Γ: Context, term: CheckableTerm, t: Type): Result[Unit] = term match {
    case Inf(e) => for {
      _r <- typeInf(i, Γ, e)
      r <- if(_r == t) returnR(()) else throwError("type mismatch")
    } yield r
    case Lam(e) => t match {
      case Fun(a, b) =>
        typeChk(i + 1, Γ + (Local(i) -> HasType(a)), substChk(0, Free(Local(i)), e), b)
      case _ => throwError("type mismatch")
    }
    case _ => throwError("type mismatch")
  }

  def substInf(i: Int, r: InferableTerm, t: InferableTerm): InferableTerm = t match {
    case Ann(e, _t) => Ann(substChk(i, r, e), _t)
    case Bound(j)   => if(i == j) r else Bound(j)
    case Free(y)    => Free(y)
    case e :@: ev   => substInf(i, r, e) ::@: substChk(i, r, ev)
  }

  def substChk(i: Int, r: InferableTerm, t: CheckableTerm): CheckableTerm = t match {
    case Inf(e) => Inf(substInf(i, r, e))
    case Lam(e) => Lam(substChk(i + 1, r, e))
  }
}
