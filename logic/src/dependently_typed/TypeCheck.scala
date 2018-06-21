package dependently_typed

/**
  * Created by MiloXia.
  */
trait TypeCheck { self: AbstractSyntax with Evaluation with Quotation =>
  type Type = Value

  type Context = Map[Name, Type]

  type Result[A] = Either[String, A]

  def throwError[A](msg: String): Result[A] = Left(msg)
  def returnR[A](res: A): Result[A] = Right(res)

  def type0(Γ: Context, t: InferableTerm): Result[Type] = typeInf(0, Γ, t)

  // for inferable terms
  def typeInf(i: Int, Γ: Context, term: InferableTerm): Result[Type] = term match {
    case Ann(e, p) => for {
      _ <- typeChk(i, Γ, p, VStar)
      t = evalChkTerm(p, Nil)
      _ <- typeChk(i, Γ, e, t)
    } yield t
    case Star      => returnR(VStar)
    case Pi(d, r)  => for {
      _ <- typeChk(i, Γ, d, VStar)
     t = evalChkTerm(d, Nil)
     _ <- typeChk(i + 1, Γ + (Local(i) -> t), substChk(0, Free(Local(i)), r), VStar)
    } yield VStar
    case Free(x)   => Γ.get(x) match {
      case Some(t) => returnR(t)
      case _ => throwError("unknown identifier")
    }
    case e :@: ev => for {
      _r <- typeInf(i, Γ, e) // get & check abs type
      r <- _r match {
        case VPi(a, b) => for {
          _ <- typeChk(i, Γ, ev, a) // check var type
          } yield b(evalChkTerm(ev, Nil))
        case _ => throwError("illegal application")
      }
    } yield r
  }

  def typeChk(i: Int, Γ: Context, term: CheckableTerm, t: Type): Result[Unit] = term match {
    case Inf(e) => for {
      _r <- typeInf(i, Γ, e)
      r <- if(quote0(_r) == quote0(t)) returnR(()) else throwError("type mismatch")
    } yield r
    case Lam(e) => t match {
      case VPi(a, b) =>
        typeChk(i + 1, Γ + (Local(i) -> a), substChk(0, Free(Local(i)), e), b(vfree(Local(i))))
      case _ => throwError("type mismatch")
    }
    case _ => throwError("type mismatch")
  }

  def substInf(i: Int, r: InferableTerm, t: InferableTerm): InferableTerm = t match {
    case Ann(e, _t) => Ann(substChk(i, r, e), _t)
    case Bound(j)   => if(i == j) r else Bound(j)
    case Free(y)    => Free(y)
    case e :@: ev   => substInf(i, r, e) ::@: substChk(i, r, ev)
    case Star       => Star
    case Pi(a, b)   => Pi(substChk(i, r, a), substChk(i + 1, r, b)) //∀a.b
  }

  def substChk(i: Int, r: InferableTerm, t: CheckableTerm): CheckableTerm = t match {
    case Inf(e) => Inf(substInf(i, r, e))
    case Lam(e) => Lam(substChk(i + 1, r, e))
  }
}
