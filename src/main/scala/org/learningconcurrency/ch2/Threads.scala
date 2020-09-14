package org.learningconcurrency
package ch2

import scala.annotation.tailrec

object ThreadsMain extends App {
  val name = Thread.currentThread.getName
  println(s"I am the thread $name")
}

object ThreadsCreation extends App {
  class MyThread extends Thread {
    override def run(): Unit = {
      println("New thread running.")
    }
  }

  val t = new MyThread
  t.start()
  t.join()
  println("New thread joined.")
}

object ThreadSleep extends App {
  val t = thread {
    Thread.sleep(1000)
    println("New thread running.")
    Thread.sleep(1000)
    println("Still running.")
    Thread.sleep(1000)
    println("Completed.")
  }
  t.join()
  println("New thread joined.")
}

object ThreadsNonDeterminism extends App {
  val t = thread { println("New thread running") }
  println("...")
  println("...")
  t.join()
  println("New thread joined.")
}

object ThreadsCommunicate extends App {
  var result: String = null
  val t = thread { result = "\nTitle\n" + "=" * 5 }
  t.join()
  println(result)
}

// race condition tiem
object ThreadsUnprotectedUid extends App {
  var uidCount = 0L

  def getUniqueId() = {
    val freshUid = uidCount + 1
    uidCount = freshUid
    freshUid
  }

  def printUniqueIds(n: Int): Unit = {
    val uids = 0 until n map { _ => getUniqueId() }
    println(s"Generated uids: $uids")
  }

  val t = thread { printUniqueIds(5) }
  printUniqueIds(5)
  t.join()
}

object ThreadSharedStateAccessReordering extends App {
  for (_ <- 0 until 100000) {
    var a = false
    var b = false
    var x = -1
    var y = -1
    val t1 = thread {
      a = true
      y = if (b) 0 else 1
    }
    val t2 = thread {
      b = true
      x = if (a) 0 else 1
    }
    t1.join()
    t2.join()
    assert(!(x == 1 && y == 1), s"x = $x, y = $y")
  }
}

object SynchronizedNesting extends App {
  import scala.collection._
  private val transfers = mutable.ArrayBuffer[String]()

  def logTransfer(name: String, n: Int) = transfers.synchronized {
    transfers += s"transfer to account '$name' = $n"
  }

  class Account(val name: String, var money: Int)

  def add(account: Account, n: Int) = account.synchronized {
    account.money += n
    if (n > 10) logTransfer(account.name, n)
  }

  val jane = new Account("Jane", 100)
  val john = new Account("John", 200)
  val t1 = thread { add(jane, 5) }
  val t2 = thread { add(john, 50) }
  val t3 = thread { add(jane, 70) }
  t1.join()
  t2.join()
  t3.join()
  println(s"--- transfers ---\n$transfers")
}

object SynchronizedDeadlock extends App {
  import SynchronizedNesting.Account

  def send(a: Account, b: Account, n: Int) = a.synchronized {
    b.synchronized {
      a.money -= n
      b.money += n
    }
  }

  val a = new Account("Jack", 1000)
  val b = new Account("Jill", 2000)
  val t1 = thread { for (_ <- 0 until 100) send(a, b, 1) }
  val t2 = thread { for (_ <- 0 until 100) send(b, a, 1) }
  t1.join()
  t2.join()
  println(s"a = ${a.money}, b = ${b.money}")
}

object SynchronizedProtectedUid {
  var uidCount = 0

  def getUniqueId() = this.synchronized {
    val freshUid = uidCount + 1
    uidCount = freshUid
    freshUid
  }
}

object DeadlockFreeSend extends App {
  import SynchronizedProtectedUid.getUniqueId
  class Account(val name: String, var money: Int) {
    val uid = getUniqueId()
  }

  def send(a1: Account, a2: Account, n: Int): Unit = {
    def adjust(): Unit = {
      a1.money -= n
      a2.money += n
    }
    if (a1.uid < a2.uid)
      a1.synchronized { a2.synchronized { adjust() } }
    else
      a2.synchronized { a1.synchronized { adjust() } }
  }

  val a = new Account("Jack", 1000)
  val b = new Account("Jill", 2000)
  val t1 = thread { for (_ <- 0 until 100) send(a, b, 1) }
  val t2 = thread { for (_ <- 0 until 100) send(b, a, 1) }
  t1.join()
  t2.join()
  println(s"a = ${a.money}, b = ${b.money}")
}

import scala.collection._
object SynchronizedBadPool extends App {
  private val tasks = mutable.Queue[() => Unit]()

  val worker = new Thread {
    def poll(): Option[() => Unit] = tasks.synchronized {
      if (tasks.nonEmpty) Some(tasks.dequeue()) else None
    }

    override def run(): Unit =
      while (true) poll() match {
        case Some(task) =>  task()
        case None =>
      }
  }

  worker.setName("Worker")
  worker.setDaemon(true)
  worker.start()

  def asynchronous(body: =>Unit) = tasks.synchronized {
    tasks.enqueue(() => body)
  }

  asynchronous { println("Hello") }
  asynchronous { println(" world!") }
  Thread.sleep(5000)
}

object SynchronizedGuardedBlocks extends App {
  val lock = new AnyRef
  var message: Option[String] = None
  val greeter = thread {
    lock.synchronized {
      while (message.isEmpty) lock.wait()
      println(message.get)
    }
  }
  lock.synchronized {
    message = Some("Hello")
    lock.notify()
  }
  greeter.join()
}

object SynchronizedPool extends App {
  private val tasks = mutable.Queue[() => Unit]()

  object Worker extends Thread {
    setDaemon(true)

    def poll() = tasks.synchronized {
      while (tasks.isEmpty) tasks.wait()
      tasks.dequeue()
    }

    override def run() = while (true) {
      val task = poll()
      task()
    }
  }

  Worker.start()
  def asynchronous(body: => Unit) = tasks.synchronized {
    tasks.enqueue(() => body)
    tasks.notify()
  }
  asynchronous { print("Hello") }
  asynchronous { println(" World!") }
  Thread.sleep(500)
}

object GracefulShutdownPool extends App {
  private val tasks = mutable.Queue[() => Unit]()

  object Worker extends Thread {
    var terminated = false

    def poll(): Option[() => Unit] = tasks.synchronized {
      while (tasks.isEmpty && !terminated) tasks.wait()
      if (!terminated) Some(tasks.dequeue()) else None
    }

    @tailrec
    override def run() = poll() match {
      case Some(task) =>
        task()
        run()
      case None =>
    }

    def shutdown() = tasks.synchronized {
      terminated = true
      tasks.notify()
    }
  }

  Worker.start()
  def asynchronous(body: => Unit) = tasks.synchronized {
    tasks.enqueue(() => body)
    tasks.notify()
  }
  asynchronous { print("Hello") }
  asynchronous { println(" World!") }
  Thread.sleep(500)
  Worker.shutdown()
}

class Page(val txt: String, var position: Int)
object Volatile extends App {
  val pages = for (i <- 1 to 5) yield {
    new Page("Na" * (100 - 20 * i) + " Batman!", -1)
  }
  @volatile var found = false
  for (p <- pages) yield thread {
    var i = 0
    while (i < p.txt.length && !found)
      if (p.txt(i) == '!') {
        p.position = i
        found = true
      } else i += 1
  }
  while (!found) {}
  println(s"results: ${pages.map(_.position)}")
}
