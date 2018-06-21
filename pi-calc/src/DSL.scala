import core.{Evaluation, Syntax}

object DSL extends Syntax with Evaluation {
  def agent(_agent: => Agent) = Agent(_agent)
  def channel[T](tag: String) = Channel[T](tag)
  def channel[T] = Channel[T]("channel-" + System.nanoTime())
  def name[T] = Name[T]
  def name[T](value: T) = Name(value)
  def action(procedure: => Unit) = Silent(procedure _)
  def nv[T](name: Name[T])(f: Name[T] => Agent) = agent(f(name))
  def _match(condition: => Boolean)(agent: => Agent) = new MatchAgent(condition _, agent _)

  def exe(agent: Agent) = new Executor(agent).start

  def main (args: Array[String]) {
    val a = channel[Channel[String]]("a")
    val b = channel[String]("b")

    //C = (ν p)(ν x) a(p).p<x>
//    val C: Agent = agent {
//      val p = name[Channel[String]]
//      a(p) * (p ! "message")
//    }
//    val C: Agent =
//      nv(name[Channel[String]]) { p =>
//        nv(name("message")) { msg =>
//          a(p) * (p ! msg)
//        }
//      }
    val C: Agent = {
      (p: Name[Channel[String]]) =>
        (msg: Name[String]) =>
          a(p) * (p ! msg)
    }.apply(name[Channel[String]])(name("message"))

    //S = a<b>.S
    lazy val S: Agent = agent {
      (a ! b) * S
    }
    //P = (ν msg) b(msg).act.P
//    lazy val P: Agent = agent {
//      val msg = name[String]
//      val act = action { println(msg.value) }
//      b(msg) * act * P
//    }
    lazy val P: Agent = nv(name[String]) { msg =>
      val act = action { println(msg.value) }
      b(msg) * act * P
    }

    //exe(C | S | P)

    lazy val P1: Agent = NilAgent()//agent(P1)
    lazy val P2: Agent = NilAgent()//agent(P2)
    lazy val Q1: Agent = NilAgent()//agent(Q1)
    lazy val Q2: Agent = NilAgent()//agent(Q2)

    val A: Agent = agent {
      val ignore = name[Channel[String]]
      val act1 = action{println("P1")}
      val act2 = action{println("P2")}

      a(ignore) * act1 * P1 + (b ! "ignore") * act2 * P2
    }
    val B: Agent = agent {
      val ignore1 = channel[String]("ignore")
      val ignore2 = name[String]
      val act1 = action{println("Q1")}
      val act2 = action{println("Q2")}

      (a ! ignore1) * act1 * Q1 + b(ignore2) * act2 * Q2
    }
    //exe(A | B)


    class Factorial(value: Int) {
      var n = value
      var result = 1

      val step = action {
        result = result * n
        n = n-1
      }
      lazy val F: Agent = agent(step * _match(n > 1)(F) )
    }
    val factor = new Factorial(5)
    exe(factor.F)
    println(factor.result)
  }
}
