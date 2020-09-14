package org.learningconcurrency.ch4

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.Try

object FuturesCreate extends App {
  Future {
    log("The future is here")
  }
  log("the future is coming")
  Thread.sleep(1000)
}

import scala.io.Source

object FuturesDataType extends App {
  val buildFile: Future[String] = Future {
    val f = Source.fromFile("build.sbt")
    try f.getLines().mkString("\n") finally f.close()
  }
  log(s"started reading the build file asynchronously")
  log(s"status ${buildFile.isCompleted}")
  Thread.sleep(250)
  log(s"status: ${buildFile.isCompleted}")
  log(s"value: ${buildFile.value}")
}

object FuturesCallback extends App {
  def getUrlSpec(): Future[List[String]] = Future {
    val url = "http://www.w3.org/Addressing/URL/url-spec.txt"
    val f = Source.fromURL(url)
    try f.getLines().toList finally f.close()
  }

  val urlSpec: Future[List[String]] = getUrlSpec()

  def find(lines: List[String], keyword: String): String =
    lines.zipWithIndex collect {
      case (line, n) if line.contains(keyword) => (n, line)
    } mkString "\n"

  urlSpec foreach { lines => log(find(lines, "telnet")) }
  urlSpec foreach { lines => log(find(lines, "password")) }
  log("callback registered, continuing with other work")
  Thread.sleep(2000)
}

object FuturesFailure extends App {
  val urlSpec: Future[String] = Future {
    val invalidUrl = "http://www.w3.org/non-existent-url-spec.txt"
    Source.fromURL(invalidUrl).mkString
  }
  urlSpec.failed foreach {
    case t => log(s"exception occurred - $t")
  }
  Thread.sleep(1000)
}

object FuturesTry extends App {
  val threadName: Try[String] = Try(Thread.currentThread.getName)
  val someText: Try[String] = Try("Try objects are synchronous")
  val message: Try[String] = for {
    tn <- threadName
    st <- someText
  } yield s"Message $st was created on t = $tn"
  handleMessage(message)
}

object FuturesNonFatal extends App {
  val f = Future { throw new InterruptedException }
  val g = Future { throw new IllegalArgumentException }
  f.failed foreach { t => log(s"error - $t") }
  g.failed.foreach(t => log(s"error - $t"))
}

import java.io._
import org.apache.commons.io.FileUtils._
import scala.jdk.CollectionConverters.IteratorHasAsScala

object FuturesClumsyCallback extends App {
  def blacklistFile(name: String): Future[List[String]] = Future {
    val src = Source.fromFile(name)
    val lines = src.getLines
    src.close()
    lines.filter(x => !x.startsWith("#") && !x.isEmpty).toList
  }

  def findFiles(patterns: List[String]): List[String] = {
    val root = new File(".")
    for {
      f <- iterateFiles(root, null, true).asScala.toList
      pat <- patterns
      abspat = root.getCanonicalPath + File.separator + pat if f.getCanonicalPath.contains(abspat)
    } yield f.getCanonicalPath
  }

  def blacklisted(name: String): Future[List[String]] =
    blacklistFile(name).map(findFiles)

  blacklistFile(".gitignore") foreach {
    case lines =>
      val files = findFiles(lines)
      log(s"matches: ${files.mkString("\n")}")
  }

  val buildFile = Future { Source.fromFile("build.sbt").getLines }

  val longest = for (ls <- buildFile) yield ls.maxBy(_.length)
  longest foreach (line => log(s"longest line: $line"))

  val netiquetteUrl = "http://www.ietf.org/rfc/rfc1855.txt"
  val netiquette = Future { Source.fromURL(netiquetteUrl).mkString }
  val urlSpecUrl = "http://www.w3.org/Addressing/URL/url-spec.txt"
  val urlSpec = Future { Source.fromURL(urlSpecUrl).mkString }
  val answer = for {
    nettext <- netiquette
    urltext <- urlSpec
  } yield {
    "First, read this: " + nettext + ". Now, try this: " + urltext
  }
  answer.foreach(log(_))

  Thread.sleep(2000)
}

import scala.concurrent.duration._
object BlockingAwait extends App {
  val urlSpecSizeFuture = Future {
    val specUrl = "http://www.w3.org/Addressing/URL/url-spec.txt"
    Source.fromURL(specUrl).size
  }
  val urlSpecSize = Await.result(urlSpecSizeFuture, 10.seconds)
  log(s"url spec contains $urlSpecSize characters")
}
