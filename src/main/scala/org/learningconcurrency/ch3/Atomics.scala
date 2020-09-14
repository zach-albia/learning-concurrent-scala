package org.learningconcurrency.ch3

import java.util.concurrent.atomic._

import scala.annotation.tailrec

object AtomicUid extends App {
  private val uid = new AtomicLong(0L)

  def getUniqueId(): Long = uid.incrementAndGet()

  execute {
    log(s"Uid asynchronously: ${getUniqueId()}")
  }

  log(s"Got a unique id: ${getUniqueId()}")
}

object CasUid {
  private val uid = new AtomicLong(0L)

  @tailrec def getUniqueId(): Long = {
    val oldUid = uid.get
    val newUid = oldUid + 1
    if (uid.compareAndSet(oldUid, newUid)) newUid
    else getUniqueId()
  }
}

object AtomicLock extends App {
  private val lock = new AtomicBoolean(false)

  def mySynchronized(body: => Unit): Unit = {
    while (!lock.compareAndSet(false, true)) {}
    try body finally lock.set(false)
  }

  var count = 0
  for (_ <- 0 until 10) execute { mySynchronized { count += 1 } }
  Thread.sleep(500)
  log(s"Count is: $count")
}
