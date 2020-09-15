package org.learningconcurrency.ch4

import java.io.File

import org.apache.commons.io.monitor.{FileAlterationListenerAdaptor, FileAlterationMonitor, FileAlterationObserver}

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global


object FileMonitor extends App {
  def fileCreated(directory: String): Future[String] = {
    val p = Promise[String]
    val fileMonitor = new FileAlterationMonitor(1000)
    val observer = new FileAlterationObserver(directory)
    val listener = new FileAlterationListenerAdaptor {
      override def onFileCreate(file: File): Unit =
        try p.trySuccess(file.getName) finally fileMonitor.stop()
    }
    observer.addListener(listener)
    fileMonitor.addObserver(observer)
    fileMonitor.start()
    p.future
  }

  fileCreated(".") foreach {
    filename => log(s"Detected new file '$filename'")
  }
}
