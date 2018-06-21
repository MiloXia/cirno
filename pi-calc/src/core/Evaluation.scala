package core

import java.util.concurrent.{Executors, ExecutorService}
import scala.collection.mutable
import scala.util.Random.shuffle
/**
  *
  */
trait Evaluation { self: Syntax =>

  //blocked producers-and-consumers with one element queue
  class Buffer[T](tag: String) extends Logger {
    private var empty = true
    private var blocked = false //the phase of wait writer exit/end
    private var store: T = _
    private var writer: Thread = null
    private var reader: Thread = null


    private def waitUntil(cond: => Boolean, afterNotify: => Unit = ()) {
      while (!cond) { wait; afterNotify }
    }

    def send(value: T): Boolean = {
      synchronized {
        lazy val _info = if(value.isInstanceOf[Channel[_]]) "channel: " + value.asInstanceOf[Channel[_]].tag else value
        debug(Thread.currentThread.getName + " " + tag + " send " + _info + " start")
        waitUntil(empty && (writer == null || writer == Thread.currentThread))
        writer = Thread.currentThread
        store = value
        empty = false
        notifyAll //notify all readers
        debug(Thread.currentThread.getName + " " + tag + " send " + _info + " end")
        waitUntil(empty)
        writer = null
        blocked = false
        debug(Thread.currentThread.getName + " " + tag + " send " + _info + " exit")
        notifyAll //notify all writers
      }
      true
    }

    def recv: T = {
      synchronized {
        reader = Thread.currentThread
        debug(Thread.currentThread.getName + " " + tag + " recv start")
        waitUntil(!empty, afterNotify = {
          if(reader == null) reader = Thread.currentThread //remark
        })
        val temp = store
        empty = true
        reader = null
        blocked = true
        lazy val _info = if(temp.isInstanceOf[Channel[_]]) "channel: " + temp.asInstanceOf[Channel[_]].tag else temp
        debug(Thread.currentThread.getName + " " + tag + " recv " + _info + " end")
        notifyAll
        temp
      }
    }

    /** try once no wait until
      * 1. must have reader & reader not current thread
      * 2. must no writer or writer is current
      * 3. must empty
      * if !(1 && 2 && 3) then mark writer & wait 10 ms
      * if (1 && 2 && 3) then send else reset & notifyAll (may block other writer cause writer != Thread.currentThread (be marked))
      */
    def trySend(value: T): Boolean = {
      def hasReader = reader != null && reader != Thread.currentThread
      synchronized {
        var mark = false
        val temp = writer
        if ((writer == null || empty) && !hasReader) {
          writer = Thread.currentThread
          mark = true
          debug(Thread.currentThread.getName + " " + tag + " try send " + " start")
          wait(10)
        }
        if (empty && (writer == null || writer == Thread.currentThread) && hasReader) {
          lazy val _info = if(value.isInstanceOf[Channel[_]]) "channel: " + value.asInstanceOf[Channel[_]].tag else value
          debug(Thread.currentThread.getName + " " + tag + " try send " + _info + " end")
          return send(value)
        } else {
          if(mark && writer == Thread.currentThread) {
            writer = temp //reset
            notifyAll //notify other wait writer
          }
        }
      }
      return false
    }

    /** try once no wait until
      * 1. must have writer & writer not current thread
      * 2. must not blocked
      * 3. must not empty
      * if !(1 && 2 && 3) then mark reader & wait 10 ms
      * if (1 && 2 && 3) then read else reset
      */
    def tryRecv: Option[T] = {
      def hasWriter = writer != null && writer != Thread.currentThread
      synchronized {
        var mark = false
        val temp = reader
        if (!(!blocked && hasWriter && !empty)) {
          reader = Thread.currentThread
          mark = true
          debug(Thread.currentThread.getName + " " + tag + " try recv " + " start")
          wait(10)
        }
        if (!blocked && hasWriter && !empty) {
            val res = recv
            lazy val _info = if(res.isInstanceOf[Channel[_]]) "channel: " + res.asInstanceOf[Channel[_]].tag else res
            debug(Thread.currentThread.getName + " " + tag + " try recv " + _info + " end")
            return Some(res)
        } else {
          if(mark && reader == Thread.currentThread) reader = temp
        }
      }
      return None
    }
  }


  class Executor (val agent: Agent) {

    private val executor: ExecutorService = Executors.newCachedThreadPool
    private val channels: mutable.Map[Channel[_], Buffer[_]] = new mutable.HashMap()
    private val countLock: AnyRef = new Object()
    private var threadCount = 0

    def start {
      executeInNewThread(agent)
      waitAllThreads()
      executor.shutdown()
    }

    private def executeInNewThread(agent: Agent) {
      increaseThreadCount()
      executor.execute(() => { executeAgent(agent); decreaseThreadCount() })
    }

    private def waitAllThreads() {
      countLock.synchronized {
        while (threadCount != 0) countLock.wait;
      }
    }

    private def increaseThreadCount() {
      countLock.synchronized {
        threadCount += 1
        countLock.notify
      }
    }

    private def decreaseThreadCount() {
      countLock.synchronized {
        threadCount -= 1
        countLock.notify
      }
    }

    private def bind[T](channel: Channel[T]): Unit = {
      channels.synchronized {
        if (!channels.keySet.contains(channel)) {
          channels += channel -> new Buffer[T](channel.tag)
        }
      }
    }

    def send[T](channel: Channel[T], name:Name[T]) {
      bind(channel)
      channels(channel).asInstanceOf[Buffer[T]].send(name.value)
    }

    def trySend[T](channel: Channel[T], name:Name[T]) = {
      bind(channel)
      channels(channel).asInstanceOf[Buffer[T]].trySend(name.value)
    }


    def recv[T](channel: Channel[T], name:Name[T]) {
      bind(channel)
      name := channels(channel).recv.asInstanceOf[T]
    }

    def tryRecv[T](channel: Channel[T], name:Name[T]): Boolean = {
      bind(channel)
      channels(channel).tryRecv match {
        case Some(value) => name := value.asInstanceOf[T]; true
        case None        => false
      }
    }


    private def executePrefix(prefix: Prefix): Unit = prefix match {
      case Silent(procedure)     => procedure.apply
      case Output(channel, name) => send(channel, name)
      case Input(channel, name)  => recv(channel, name)
      case ConcatenationPrefix(left, right) => executePrefix(left.apply)
        executePrefix(right.apply)
    }

    private def executeAgent(agent: Agent): Unit = {
      agent match {

        case NilAgent() => ()

        case RestrictedAgent(_agent) => executeAgent(_agent.apply)

        case PrefixAgent(left, right) => executePrefix(left.apply)
          executeAgent(right.apply)

        case MatchAgent(condition, _then) => if (condition.apply) executeAgent(_then.apply)

        case ParallelAgent(left, right) => executeInNewThread(left.apply)
          executeInNewThread(right.apply)

        //choose one of many agents
        case SummationAgent(left, right) => {
          val agents = sumTerms(left.apply) ::: sumTerms(right.apply)
          var continue: Agent = null
          while (continue == null) {
            val random = shuffle(agents)
            random.foreach { agent =>
              if (continue == null) {
                continue = runGuard(agent.left.apply, agent.right.apply)
              }
            }
          }
          executeAgent(continue)
        }

      }
    }

    private def runGuard(prefix: Prefix, cont: Agent): Agent = prefix match {
      case Silent(procedure) =>
        procedure.apply
        cont

      case Output(channel, name) =>
        if (trySend(channel, name)) cont
        else null

      case Input(channel, name) =>
        if (tryRecv(channel, name)) cont
        else null

      case concatenationPrefix: ConcatenationPrefix =>
        splitPrefix(concatenationPrefix) match {
          case (head, rest) => runGuard(head, PrefixAgent(() => rest, () => cont))
        }
    }

    //list of all agents in a summation.
    private def sumTerms(agent: Agent): List[PrefixAgent] = {
      agent match {
        case agent: PrefixAgent => agent :: Nil
        case SummationAgent(left, right) => sumTerms(left.apply) ::: sumTerms(right.apply)
      }
    }

    private def splitPrefix(concatenationPrefix: ConcatenationPrefix): (Prefix, Prefix) = {
      (head(concatenationPrefix), rest(concatenationPrefix).getOrElse(concatenationPrefix.right.apply))
    }

    private def head(prefix: Prefix): Prefix = {
      prefix match {
        case ConcatenationPrefix(left, right) => head(left.apply)
        case otherPrefix => otherPrefix
      }
    }

    private def rest(prefix: Prefix): Option[Prefix] = {
      prefix match {
        case ConcatenationPrefix(left, right) => rest(left.apply) match {
          case Some(concatPrefix) => Some(ConcatenationPrefix(() => concatPrefix, right))
          case _ => Some(right.apply)
        }
        case otherPrefix => None
      }
    }
  }
}
