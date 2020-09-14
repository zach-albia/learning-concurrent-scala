package org.learningconcurrency.ch3

import java.util.concurrent.{ConcurrentHashMap, LinkedBlockingQueue}
import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec
import scala.collection._
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala

object CollectionsBad extends App {
  val buffer = mutable.ArrayBuffer[Int]()

  def asyncAdd(numbers: Seq[Int]) = execute {
    buffer ++= numbers
    log(s"buffer = $buffer")
  }

  asyncAdd(0 until 10)
  asyncAdd(10 until 20)
  Thread.sleep(500)
}

class AtomicBuffer[T] {
  private val buffer = new AtomicReference[List[T]](Nil)

  @tailrec final def +=(x: T): Unit = {
    val xs = buffer.get
    val nxs = x :: xs
    if (!buffer.compareAndSet(xs, nxs)) this += x
  }
}

/**
 * Tip:
 *
 * Use iterators on concurrent data structures only when you can ensure that no
 * other thread will modify the data structure from the point where the iterator
 * was created until the point where the iterator's hasNext method returns false.
 */
object CollectionsIterators extends App {
  val queue = new LinkedBlockingQueue[String]
  for (i <- 1 to 5500) queue.offer(i.toString)
  execute {
    val it = queue.iterator()
    while (it.hasNext) log(it.next())
  }
  for (_ <- 1 to 5500) queue.poll()
  Thread.sleep(1000)
}

object CollectionsConcurrentMapBulk extends App {
  val names = new ConcurrentHashMap[String, Int]().asScala
  names("Johnny") = 0
  names("Jane") = 0
  names("Jack") = 0
  execute {
    for (n <- 0 until 10) names(s"John $n") = n
  }
  execute {
    for (n <- names) log(s"name: $n")
  }
  Thread.sleep(1000)
}

object CollectionsTrieMapBulk extends App {
  val names = new TrieMap[String, Int]
  names("Janice") = 0
  names("Jackie") = 0
  names("Jill") = 0
  execute {
    for (n <- 0 until 100) names(s"John $n") = n
  }
  execute {
    log("snapshot time")
    for (n <- names.keys.toSeq.sorted) log(s"name: $n")
  }
  Thread.sleep(1000)
}
