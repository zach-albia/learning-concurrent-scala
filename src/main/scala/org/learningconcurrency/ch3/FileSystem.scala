package org.learningconcurrency.ch3

import java.io.File
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicReference

import org.apache.commons.io.FileUtils

import scala.annotation.tailrec
import scala.collection.concurrent
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters._

object FileSystem extends App {
  val fileSystem = new FileSystem(".")
  fileSystem.logMessage("Testing log!")
  fileSystem.deleteFile("test.txt")

  val rootFiles = fileSystem.allFiles()
  log("All files in the root dir: " + rootFiles.mkString(", "))

  Thread.sleep(1000)
}

class FileSystem(val root: String) {

  private val messages = new LinkedBlockingQueue[String]()
  val logger = new Thread {
    setDaemon(true)
    override def run(): Unit = while (true) log(messages.take())
  }
  logger.start()
  def logMessage(msg: String): Unit = messages.offer(msg)

  val rootDir = new File(root)
  val files: concurrent.Map[String, Entry] = new TrieMap[String, Entry]
  for (f <- FileUtils.iterateFiles(rootDir, null, false).asScala)
    files.put(f.getName, new Entry(false))

  class Entry(val isDir: Boolean) {
    val state = new AtomicReference[State](new Idle)
  }

  sealed trait State
  class Idle extends State
  class Creating extends State
  class Copying(val n: Int) extends State
  class Deleting extends State

  @tailrec private def prepareForDelete(entry: Entry): Boolean = {
    val s0 = entry.state.get
    s0 match {
      case _: Idle =>
        if (entry.state.compareAndSet(s0, new Deleting)) true
        else prepareForDelete(entry)
      case _: Creating =>
        logMessage("File currently being created, cannot delete.")
        false
      case _: Copying =>
        logMessage("File currently copying, cannot delete.")
        false
      case _: Deleting =>
        false
    }
  }

  def deleteFile(filename: String): Unit = {
    files.get(filename) match {
      case None =>
        logMessage(s"Path '$filename' does not exist!")
      case Some(entry) if entry.isDir =>
        logMessage(s"Path '$filename' is a directory!")
      case Some(entry) => execute {
        if (prepareForDelete(entry))
          if (FileUtils.deleteQuietly(new File(filename)))
            files.remove(filename)
      }

    }
  }

  @tailrec
  private def acquire(entry: Entry): Boolean = {
    val s0 = entry.state.get
    s0 match {
      case _: Creating | _: Deleting =>
        logMessage("File inaccessible, cannot copy."); false
      case _: Idle =>
        if (entry.state.compareAndSet(s0, new Copying(1))) true
        else acquire(entry)
      case c: Copying =>
        if (entry.state.compareAndSet(s0, new Copying(c.n + 1))) true
        else acquire(entry)
    }
  }

  @tailrec
  private def release(entry: Entry): Unit = {
    val s0 = entry.state.get
    s0 match {
      case _: Creating =>
        if (!entry.state.compareAndSet(s0, new Idle)) release(entry)
      case c: Copying =>
        val nstate = if (c.n == 1) new Idle else new Copying(c.n - 1)
        if (!entry.state.compareAndSet(s0, nstate)) release(entry)
    }
  }

  def copyFile(src: String, dest: String): Unit = {
    files.get(src) match {
      case Some(srcEntry) if !srcEntry.isDir => execute {
        if (acquire(srcEntry)) try {
          val destEntry = new Entry(isDir = false)
          destEntry.state.set(new Creating)
          if (files.putIfAbsent(dest, destEntry).isEmpty) try {
            FileUtils.copyFile(new File(src), new File(dest))
          } finally release(destEntry)
        } finally release(srcEntry)
      }
    }
  }

  def allFiles(): Iterable[String] = for ((name, _) <- files) yield name
}
