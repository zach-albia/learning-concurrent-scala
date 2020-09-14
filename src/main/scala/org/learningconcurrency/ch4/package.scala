package org.learningconcurrency

import scala.util.{Failure, Success, Try}

package object ch4 {
  def log[A](a: A) = println(a)

  def handleMessage(t: Try[String]) = t match {
    case Success(msg) => log(msg)
    case Failure(error) => log(s"unexpected failure - $error")
  }
}
