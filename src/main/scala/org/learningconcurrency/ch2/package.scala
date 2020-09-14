package org.learningconcurrency

package object ch2 {
  val concurrency = Runtime.getRuntime.availableProcessors

  def thread(body: => Unit): Thread = {
    val t = new Thread {
      override def run(): Unit = body
    }
    t.start()
    t
  }
}
