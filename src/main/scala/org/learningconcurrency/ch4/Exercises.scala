package org.learningconcurrency.ch4

import java.util.concurrent.ConcurrentHashMap

import scala.async.Async.{async, await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise, blocking}
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Success

object Exercise1 extends App {
  print("Enter your URL: ")
  val userUrl = readLine()
  val donePromise = Promise[Unit]
  val dotFuture = Future {
    while (!donePromise.isCompleted) {
      Thread.sleep(50)
      print(".")
    }
  }
  val timeoutDuration = 2000
  val timeoutFuture = timeout(timeoutDuration).map { _ =>
    donePromise.trySuccess(())
    println("Request timed out!")
    Iterator.empty
  }
  val urlHtmlFuture = Future {
    val value = Source.fromURL(userUrl).getLines
    donePromise.success(())
    value
  }

  Await.ready(for {
    _ <- dotFuture
    it <- urlHtmlFuture.or(timeoutFuture)
    _ <- donePromise.future
  } yield {
    println("\n")
    it.foreach(println)
  }, Duration.Inf)
}

object Exercise2 extends App {

  class IVar[T] {
    private val promise = Promise[T]

    def apply(): T =
      if (promise.isCompleted)
        Await.result(promise.future, Duration.Inf)
      else throw new IllegalStateException

    def :=(x: T): Unit =
      promise.success(x)
  }

  val a = new IVar[Int]
  a := 4
  println(a())
}

object Exercise3 {

  implicit class FutureOps[T](f: Future[T]) {
    def exists(p: T => Boolean): Future[Boolean] = f.map(p)
  }

}

object Exercise4 {

  implicit class FutureOps[T](f: Future[T]) {
    def exists(pred: T => Boolean): Future[Boolean] = {
      val p = Promise[Boolean]
      f.onComplete { case Success(t) => p.success(pred(t)) }
      p.future
    }
  }

}

object Exercise5 {

  implicit class FutureOps[T](f: Future[T]) {
    def exists(pred: T => Boolean): Future[Boolean] = async {
      pred(await(f))
    }
  }

}

object Exercise6 {

  import sys.process._

  def spawn(command: String): Future[Int] = Future {
    blocking {
      command.!
    }
  }
}

object Exercise7 {

  class IMap[K, V] {

    import scala.collection.concurrent
    import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala

    val map: concurrent.Map[K, Promise[V]] = new ConcurrentHashMap[K, Promise[V]]().asScala

    def update(k: K, v: V): Unit = map.get(k).fold {
      map(k) = Promise.successful(v)
    } {
      _.success(v)
    }

    def apply(k: K): Future[V] = map.get(k).fold {
      map(k) = Promise[V]()
      map(k).future
    } {
      _.future
    }
  }

}

object Exercise8 {

  import concurrent.ExecutionContext.Implicits.global

  implicit class PromiseOps[T](promise: Promise[T]) {
    def compose[S](f: S => T): Promise[S] = {
      val np = Promise[S]
      np.future.map(f.andThen(promise.success))
      np
    }
  }

}
