package core

/**
  * for debug info
  */
trait Logger {
  val DEBUG = 1
  val INFO  = 2
  val level = INFO
  def debug(info: => String): Unit = {
    if(level == DEBUG) println("DEBUG - " + info)
  }
}
