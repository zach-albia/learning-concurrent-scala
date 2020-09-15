package org.learningconcurrency.ch4

import scala.async.Async._
import scala.concurrent.{Await, Future, blocking}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Async extends App {

  def delay(n: Int): Future[Unit] = async {
    blocking {
      Thread.sleep(n * 1000)
    }
  }

  // this is...
  async {
    log("T-minus 1 second")
    await {
      delay(1)
    }
    log("done")
  }

  // ...roughly equivalent to:
  Future {
    log("T-minus 1 second")
    delay(1) foreach { _ => log("done") }
  }

  def countdown(n: Int)(f: Int => Unit): Future[Unit] = async {
    var i = n
    while (i > 0) {
      f(i)
      await { delay(1) }
      i -= 1
    }
  }

  val cd = countdown(10) { n => log(s"T-minus $n seconds") }
  cd foreach {
    _ => log(s"This program is over!")
  }
  Await.ready(cd, 15.seconds)
}
