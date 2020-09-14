package org.learningconcurrency.ch3

import scala.concurrent._
import java.util.concurrent.ForkJoinPool

object ExecutorsCreate extends App {
  val executor = new ForkJoinPool
  executor.execute(() => log("This task is run asynchronously."))
  Thread.sleep(500)
}

object ExecutionContextGlobal extends App {
  val ectx = ExecutionContext.global
  ectx.execute(() => log("Running on the execution context."))
  Thread.sleep(500)
}

object ExecutionContextCreate extends App {
  val pool = new ForkJoinPool(2)
  val ectx = ExecutionContext.fromExecutorService(pool)
  ectx.execute(() => log("Running on the execution context again."))
  Thread.sleep(500)
}

object ExecutionContextSleep extends App {
  for (i <- 0 until 32) execute {
    Thread.sleep(2000)
    log(s"Task $i completed.")
  }
  Thread.sleep(10000)
}
