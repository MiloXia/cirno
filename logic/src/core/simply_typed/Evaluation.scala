package core.simply_typed

/**
  * Evaluation in λ →
  *     e ⇓ v
  *   ----------  -----
  *   e :: τ ⇓ v  x ⇓ x
  *
  *   e ⇓ λx → v  v[x |→ e' ] ⇓ v'
  *   ------------------------------
  *          e e' ⇓ v'
  *
  *   e ⇓ n  e' ⇓ v'
  *   --------------
  *    e e' ⇓ n v'
  *
  *      e ⇓ v
  *   ---------------
  *   λx → e ⇓ λx → v
  */



trait Evaluation { self: AbstractSyntax =>

  type Env = List[Value]

  def evalInfTerm(term: InferableTerm, d: Env): Value = term match {
    case Ann(e, _) => evalChkTerm(e, d)
    case Free(x)   => vfree(x)
    case Bound(i)  => d(i)
    case e :@: ev  => vapp(evalInfTerm(e, d), evalChkTerm(ev, d))
  }

  def vapp(v1: Value, v: Value): Value = v1 match {
    case VLam(f)     => f(v)
    case VNeutral(n) => VNeutral(NApp(n, v))
  }

  def evalChkTerm(term: CheckableTerm, d: Env): Value = term match {
    case Inf(i) => evalInfTerm(i, d)
    case Lam(e) => VLam(x => evalChkTerm(e, x :: d))
  }
}
