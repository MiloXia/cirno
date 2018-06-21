package core.simply_typed


object Test extends AbstractSyntax with TypeCheck with Evaluation with Quotation {

  def main(args: Array[String]) {
    val id = Lam(Inf(Bound(0)))
    val const = Lam(Lam(Inf(Bound(1))))

    val term1 = Ann(id, Fun(tfree("a"), tfree("a"))) ::@: free("y")
    val term2 = (Ann(const,
      Fun(Fun(tfree("b"), tfree("b")),
        Fun(tfree("a"),
          Fun(tfree("b"), tfree("b"))))) ::@: id) ::@: free("y")

    val env1: Context = Map(Global("y") -> HasType(tfree("a")), Global("a") -> HasKind(Star))
    val env2 = Map(Global("b") -> HasKind(Star)) ++ env1

    val res1 = quote0(evalInfTerm(term1, List()))
    val res2 = quote0(evalInfTerm(term2, List()))

    val res3 = type0(env1, term1)
    val res4 = type0(env2, term2)

    println(res1)
    println(res2)
    println(res3)
    println(res4)

  }
}
