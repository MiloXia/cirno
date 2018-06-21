package core.simply_typed

/**
  * abstract syntax for λ → :
  * types:
  * τ ::= α        base type
  *      | τ → τ'  function type
  *
  * terms:
  *  e ::= e ::τ    annotated term 1
  *      | x         variable
  *      | e e'      application
  *      | λx → e   lambda abstraction
  *
  *  values:
  *  v ::= n         neutral term
  *      | λx → v   lambda abstraction
  *      n ::= x     variable
  *      | n v       application
  *
  */
trait AbstractSyntax {

  trait Name
  case class Global(str: String) extends Name
  case class Local(i: Int) extends Name
  case class Quote(i: Int) extends Name

  // Separating inferable and checkable terms InferableTerm & CheckableTerm

  trait InferableTerm
  case class Ann(term: CheckableTerm, tpe: Type) extends InferableTerm
  // represent locally bound variables by de Bruijn indices: id: λx → x to λ → 0 ; const: λy → λx → y to λ → λ → 1
  case class Bound(i: Int) extends InferableTerm //local var
  case class Free(name: Name) extends InferableTerm // free var
  case class :@:(inferableTerm: InferableTerm, checkableTerm: CheckableTerm) extends InferableTerm // app

  trait CheckableTerm
  case class Inf(inferableTerm: InferableTerm) extends CheckableTerm // just for type check
  case class Lam(checkableTerm: CheckableTerm) extends CheckableTerm // lambda

  trait Type
  case class TFree(name: Name) extends Type
  case class Fun(a: Type, b: Type) extends Type //a → b

  trait Value
  case class VLam(f: Value => Value) extends Value
  case class VNeutral(n: Neutral) extends Value

  trait Neutral
  case class NFree(name: Name) extends Neutral
  case class NApp(n: Neutral, v: Value) extends Neutral

  def vfree(name: Name): Value = VNeutral(NFree(name))

  def tfree(alpha: String) = TFree(Global(alpha))

  def free(x: String) = Inf(Free(Global(x)))

  //help fun for :@:
  implicit class App(c: CheckableTerm) {
    def ::@:(i: InferableTerm) = :@:(i, c)
  }
}


