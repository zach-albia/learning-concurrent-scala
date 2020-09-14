package org.learningconcurrency

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

package object ch4 {
  def log[A](a: A) = println(a)

  def handleMessage(t: Try[String]) = t match {
    case Success(msg) => log(msg)
    case Failure(error) => log(s"unexpected failure - $error")
  }

  implicit class FutureOps[T](val self: Future[T]) {
    def or(that: Future[T]): Future[T] = {
      val p = Promise[T]
      self onComplete (x => p tryComplete x)
      that onComplete (y => p tryComplete y)
      p.future
    }
  }
}
