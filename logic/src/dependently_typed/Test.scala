package dependently_typed


object Test extends AbstractSyntax with TypeCheck with Evaluation with Quotation {

  def main(args: Array[String]) {
    val id = Lam(Lam(Inf(Bound(0)))) // λt -> λa -> a
    val typeId = Inf(Pi( // ∀x:∗.∀y:x.x
      Inf(Star),
      Inf(Pi(
        Inf(Bound(0)),
        Inf(Bound(1))
      ))
    ))

    val annId = Ann(id, typeId)
    val term1 = annId ::@: free("Bool")
    val term2 = term1 ::@: free("False")

    val env1: Context = Map(Global("Bool") -> VStar)
    val env2: Context = Map(Global("False") -> vfree(Global("Bool"))) ++ env1

    val res1 = quote0(evalInfTerm(term1, List()))
    val res2 = quote0(evalInfTerm(term2, List()))

    val res3 = type0(env1, term1).map(quote0)
    val res4 = type0(env2, term2).map(quote0)

    println(res1)
    println(res2)
    println(res3)
    println(res4)

  }
}
