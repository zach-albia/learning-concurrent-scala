package org.learningconcurrency.ch4

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.io.Source
import scala.io.StdIn.readLine

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
