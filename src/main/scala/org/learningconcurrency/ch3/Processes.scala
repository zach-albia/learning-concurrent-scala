package org.learningconcurrency.ch3

import scala.sys.process._

object ProcessRun extends App {
  val command = "dir"
  val exitcode = command.!
  log(s"command exited with status $exitcode")
}
