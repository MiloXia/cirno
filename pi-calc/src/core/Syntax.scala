package core

/**
  * Prefixes α ::= ¯y<x>     Output
  *             | y(x)        Input
  *             | τ          Silent
  *
  * Agents P ::= 0            Nil
  *          | α.P           Prefix Agent
  *          | P + P          Sum
  *          | P|P            Parallel
  *          | (νx)P         Restriction
  *          | [x = y].P      Match
  *          | [x 6= y].P     Mismatch
  *
  * Definitions A(x 1 ,...,x n ) def= P
  */
trait Syntax {

  type Lazy[A] = () => A

  //pi-calculus name
  class Name[T](var value: T) {
    def :=(_value: T) { value = _value }
  }
  object Name {
    def apply[T]: Name[T] = new Name(null.asInstanceOf[T])
    def apply[T](value: T): Name[T] = new Name(value)
    //auto box & unbox
    implicit def unbox[T](name: Name[T]): T = name.value
    implicit def box[T](value: T): Name[T] = Name(value)
  }

  //pi-calculus channel, type T is the name value type
  class Channel[T](val tag: String) {
    def apply(name: Name[T]) = Input(this, name) //receive
    def <>(name:Name[T]) = Output(this, name) //send
    def !(name:Name[T]) = <>(name)
  }
  object Channel {
    def apply[T](tag: String) = new Name(new Channel[T](tag)) //channel is a name
  }

  //Prefix
  trait Concatenation { self: Prefix =>
    //concatenation operator
    def *(other: => Prefix) = ConcatenationPrefix(() => this, other _)
    def *(other: => Agent) = PrefixAgent(() => this, other _)
  }
  trait Prefix extends Concatenation
  case class Output[T](channel: Channel[T], name: Name[T]) extends Prefix
  case class Input[T](channel: Channel[T], name: Name[T]) extends Prefix
  case class Silent(procedure: Lazy[Unit]) extends Prefix
  case class ConcatenationPrefix(left: Lazy[Prefix], right: Lazy[Prefix]) extends Prefix
  object Prefix {
    //default concatenation operator.
    implicit def PrefixToAgent(prefix: Prefix): PrefixAgent = PrefixAgent(() => prefix, NilAgent)
  }

  //Agent
  trait Composition { this: Agent =>
    def |(other: => Agent) = ParallelAgent(() => this, other _)
  }
  trait Summation { self: Agent =>
    def +(other: => PrefixAgent) = SummationAgent(() => this, other _)
  }
  trait Agent extends Composition
  case class NilAgent() extends Agent
  case class PrefixAgent(left: Lazy[Prefix], right: Lazy[Agent]) extends Agent with Summation
  case class RestrictedAgent(agent: Lazy[Agent]) extends Agent
  case class ParallelAgent(left: Lazy[Agent], right: Lazy[Agent])extends Agent
  case class SummationAgent(left: Lazy[Agent], right: Lazy[Agent]) extends Agent with Summation
  case class MatchAgent(condition: Lazy[Boolean], _then: Lazy[Agent]) extends Agent

  object Agent {
    def apply(agent: => Agent) = RestrictedAgent(agent _)
  }
}
