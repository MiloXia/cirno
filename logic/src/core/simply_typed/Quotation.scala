package core.simply_typed

/**
  *  higher-order abstract syntax requires us to define a
  *  quote function that takes a Value back to a term.
  *  quote:
  *  λ | -> λ | -> λ | ... | -> k (Bound(k))
  *  0  |    1 |     2 | ... | -> i ++
  *  index = i - k - 1
  */
trait Quotation { self: AbstractSyntax =>
  def quote0: Value => CheckableTerm = quote(0)

  def quote(i: Int)(v: Value): CheckableTerm = v match {
    case VLam(f) => Lam(quote(i + 1)(f(vfree(Quote(i)))))
    case VNeutral(n) => Inf(neutralQuote(i, n))
  }

  def neutralQuote(i: Int, ne: Neutral): InferableTerm = ne match {
    case NFree(x) => boundfree(i, x)
    case NApp(n, v) => neutralQuote(i, n) ::@: quote(i)(v)
  }

  def boundfree(i: Int, name: Name): InferableTerm = name match {
    case Quote(k) => Bound(i - k - 1)
    case x => Free(x)
  }


}
