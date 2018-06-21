package dependently_typed

/**
  * abstract syntax for λ → :
  *  terms:
  *  e, ρ,κ ::= e :: ρ       annotated term
  *           | ∗              the type of types
  *           | ∀x :: ρ.ρ'    dependent function space
  *           | x              variable
  *           | e e 0          application
  *           | λx → e        lambda abstraction
  *
  *  values:
  *  v, t ::= n           neutral term
  *      | ∗              the type of types
  *      | ∀x :: t.t'     dependent function space
  *      | λx → v        lambda abstraction
  *
  */
trait AbstractSyntax {

  trait Name
  case class Global(str: String) extends Name
  case class Local(i: Int) extends Name
  case class Quote(i: Int) extends Name

  // Separating inferable and checkable terms InferableTerm & CheckableTerm

  trait InferableTerm
  case class Ann(term: CheckableTerm, vTerm: CheckableTerm) extends InferableTerm
  // represent locally bound variables by de Bruijn indices: id: λx → x to λ → 0 ; const: λy → λx → y to λ → λ → 1
  case object Star extends InferableTerm // *
  case class Pi(domain: CheckableTerm, range: CheckableTerm) extends InferableTerm
  case class Bound(i: Int) extends InferableTerm //local var
  case class Free(name: Name) extends InferableTerm // free var
  case class :@:(inferableTerm: InferableTerm, checkableTerm: CheckableTerm) extends InferableTerm // app

  trait CheckableTerm
  case class Inf(inferableTerm: InferableTerm) extends CheckableTerm // just for type check
  case class Lam(checkableTerm: CheckableTerm) extends CheckableTerm // lambda

  trait Value
  case class VLam(f: Value => Value) extends Value
  case object VStar extends Value
  case class VPi(domain: Value, range: Value => Value) extends Value
  case class VNeutral(n: Neutral) extends Value

  trait Neutral
  case class NFree(name: Name) extends Neutral
  case class NApp(n: Neutral, v: Value) extends Neutral

  def vfree(name: Name): Value = VNeutral(NFree(name))
  def free(x: String) = Inf(Free(Global(x)))

  //help fun for :@:
  implicit class App(c: CheckableTerm) {
    def ::@:(i: InferableTerm) = :@:(i, c)
  }
}


