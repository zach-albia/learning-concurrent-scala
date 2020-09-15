package org.learningconcurrency

import java.util.{Timer, TimerTask}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{CancellationException, Future, Promise}
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

  private val timer = new Timer(true)

  def timeout(t: Long): Future[Unit] = {
    val p = Promise[Unit]
    timer.schedule(new TimerTask {
      def run() = {
        p.success(())
        timer.cancel()
      }
    }, t)
    p.future
  }

  type Cancellable[T] = (Promise[Unit], Future[T])

  def cancellable[T](b: Future[Unit] => T): Cancellable[T] = {
    val cancel = Promise[Unit]
    val f = Future {
      val r = b(cancel.future)
      if (!cancel.tryFailure(new Exception)) {
        throw new CancellationException
      }
      r
    }
    (cancel, f)
  }
}
