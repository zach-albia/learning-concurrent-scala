package org.learningconcurrency.ch4

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

object PromisesCreate extends App {
  val p = Promise[String]
  val q = Promise[String]
  p.future foreach { x => log(s"p succeeded with '$x'") }
  Thread.sleep(1000)
  p success "assigned"
  q failure new Exception("not kept")
  q.future.failed foreach { t => log(s"q failed with $t") }
  Thread.sleep(1000)
}

import scala.util.control.NonFatal

object PromisesCustomAsync extends App {
  def myFuture[T](b: =>T ): Future[T] = {
    val p = Promise[T]
    global.execute(() => try {
      p.success(b)
    } catch {
      case NonFatal(e) => p.failure(e)
    })
    p.future
  }
  val f = myFuture { "naa" + "na" * 8 + " Katamari Damacy!" }
  f foreach log
  Thread.sleep(100)
}
