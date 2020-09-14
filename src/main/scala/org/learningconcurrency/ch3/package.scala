package org.learningconcurrency

import scala.concurrent.ExecutionContext

package object ch3 {
  def log[A](a: A) = println(a)

  def execute(body: => Unit) = ExecutionContext.global.execute(() => body)

  def logMessage(s: String) = log(s)
}
