package org.learningconcurrency.ch2

import org.learningconcurrency.ch2.DeadlockFreeSend.Account

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object Exercises {

  def parallel[A, B](a: => A, b: => B): (A, B) = {
    var t1Result: A = null.asInstanceOf[A]
    var t2Result: B = null.asInstanceOf[B]
    val t1 = thread {
      t1Result = a
    }
    val t2 = thread {
      t2Result = b
    }
    t1.join()
    t2.join()
    (t1Result, t2Result)
  }

  def periodically(duration: Long)(b: => Unit): Unit =
    thread {
      while (true) {
        b
        Thread.sleep(duration)
      }
    }

  class SyncVar[T] {

    private val lock = new AnyRef
    private var data: Option[T] = None

    def isEmpty = lock.synchronized {
      data.isEmpty
    }

    def nonEmpty = !isEmpty

    def get(): T = lock.synchronized {
      if (data.isEmpty) {
        throw new IllegalStateException("SyncVar is empty")
      } else {
        val value = data.get
        data = None
        value
      }
    }

    def put(x: T): Unit = lock.synchronized {
      if (data.nonEmpty) {
        throw new IllegalStateException("SyncVar is non-empty")
      }
      data = Some(x)
    }

    def getWait(): T = lock.synchronized {
      while (data.isEmpty) {
        lock.wait()
      }
      val t = data.get
      data = None
      lock.notify()
      t
    }

    def putWait(x: T): Unit = lock.synchronized {
      while (data.nonEmpty) {
        lock.wait()
      }
      data = Some(x)
      lock.notify()
    }
  }

  class SyncQueue[T](size: Int) {

    import scala.collection.mutable

    private val lock = new AnyRef
    private val data = mutable.Queue[T]()

    def isEmpty = lock.synchronized {
      data.isEmpty
    }

    def nonEmpty = !isEmpty

    def getWait(): T = lock.synchronized {
      while (data.isEmpty) lock.wait()
      val t = data.dequeue()
      lock.notify()
      t
    }

    def putWait(x: T): Unit = lock.synchronized {
      while (data.size == size) lock.wait()
      data.enqueue(x)
      lock.notify()
    }

    def dump: mutable.Queue[T] = lock.synchronized {
      mutable.Queue(data.toList: _*)
    }
  }

  object DeadlockFree {

    import DeadlockFreeSend._

    def sendAll(accounts: Set[Account], target: Account): Unit =
      accounts.foreach { acc => send(acc, target, acc.money) }
  }

  object PriorityTaskPool {

    private case class Task(priority: Int, action: () => Unit)

  }

  class PriorityTaskPool(p: Int, important: Int) {

    import PriorityTaskPool.Task

    private val tasks = mutable.PriorityQueue[Task]()(Ordering.by((task: Task) => task.priority))

    private class Worker extends Thread {
      var terminated = false

      setDaemon(true)

      def poll(): Option[Task] = tasks.synchronized {
        while (tasks.isEmpty) tasks.wait()
        if (!terminated) Some(tasks.dequeue()) else None
      }

      @tailrec
      final override def run(): Unit = {
        poll() match {
          case Some(task) if !terminated || task.priority > important =>
            task.action()
            run()
          case None =>
        }
      }

      def shutdown() = tasks.synchronized {
        terminated = true
        tasks.notify()
      }
    }

    private val workers = List.fill(p)(new Worker)

    workers.foreach(_.start())

    def asynchronous(priority: Int)(task: => Unit): Unit = tasks.synchronized {
      tasks.enqueue(Task(priority, () => task))
      tasks.notify()
    }

    def shutdown(): Unit = workers.foreach(_.shutdown())
  }

  class ConcurrentBiMap[K, V] {
    private val keyMap = new mutable.HashMap[K, V]()
    private val valMap = new mutable.HashMap[V, K]()

    def put(k: K, v: V): Option[(K, V)] = keyMap.synchronized {
      valMap.synchronized {
        if (keyMap.contains(k) || valMap.contains(v)) None
        else {
          keyMap.put(k, v)
          valMap.put(v, k)
          Some((k, v))
        }
      }
    }

    def replace(k1: K, v1: V, k2: K, v2: V): Unit =
      keyMap.synchronized {
        valMap.synchronized {
          if (keyMap.exists { case (k, v) => k == k1 && v == v1 }) {
            keyMap.remove(k1)
            valMap.remove(v1)
            keyMap.put(k2, v2)
            valMap.put(v2, k2)
          }
        }
      }

    private def removeElement[A, B](kMap: mutable.Map[A, B], vMap: mutable.Map[B, A])(k: A) = kMap.synchronized {
      vMap.synchronized {
        val v = kMap.get(k)
        kMap.remove(k)
        v.foreach(vMap.remove)
        v
      }
    }

    def removeKey(k: K): Option[V] = removeElement(keyMap, valMap)(k)

    def removeValue(v: V): Option[K] = removeElement(valMap, keyMap)(v)

    def getValue(k: K): Option[V] = keyMap.synchronized {
      keyMap.get(k)
    }

    def getKey(v: V): Option[K] = valMap.synchronized {
      valMap.get(v)
    }

    def size: Int = keyMap.synchronized {
      valMap.synchronized {
        assert(keyMap.size == valMap.size)
        keyMap.size
      }
    }

    def iterator: Iterator[(K, V)] =
      keyMap.iterator

    def dump: String =
      keyMap.toString + "\n" + valMap.toString
  }

  def cache[K, V](f: K => V): K => V = {
    val map: mutable.Map[K, V] = mutable.HashMap()
    (k: K) => {
      map.synchronized {
        map.get(k) match {
          case Some(v) => v
          case None =>
            val v = f(k)
            map.put(k, v)
            v
        }
      }
    }
  }

}

import Exercises._

object ParallelTest extends App {
  println(parallel(1, 2))
  private val range: Range.Inclusive = 1 to 100000
  println(
    parallel(
      parallel(sumMath(math.tan), sumMath(math.cos)),
      parallel(sumMath(math.cos), sumMath(math.sin))
    )
  )

  private def sumMath(f: Double => Double) = {
    range.to(Iterator).map(f(_)).sum
  }
}

object PeriodicallyTest extends App {
  periodically(2000) {
    println("Hey")
  }
}

object SyncVarProducerConsumer extends App {
  val sv = new SyncVar[Int]
  val consumer = thread {
    while (true) {
      if (sv.nonEmpty) {
        println(sv.get())
      }
    }
  }
  (0 until 15) foreach { i =>
    @tailrec
    def busyPutWait(i: Int): Unit = {
      if (sv.isEmpty) {
        sv.put(i)
      } else busyPutWait(i)
    }

    busyPutWait(i)
  }
}

object SyncVarWaitProducerConsumer extends App {
  val sv = new SyncVar[Int]

  private val limit = 100000
  val consumer = thread {
    var i = 0
    while (i < limit) {
      println(sv.getWait())
      i += 1
    }
  }

  (0 until limit) foreach { i =>
    sv.putWait(i)
  }
}

object SyncQueueProducerConsumer extends App {
  val sq = new SyncQueue[Int](10)
  val limit = 100
  val consumer = thread {
    var i = 0
    while (i < limit) {
      println(sq.getWait())
      println(sq.dump)
      i += 1
    }
  }

  (0 until limit) foreach { i =>
    sq.putWait(i)
  }
}

object DeadlockFreeSendAll extends App {

  import DeadlockFree._

  val accounts1 = (1 to 100).map { i => new Account(i.toString, 1) }
  val accounts2 = (101 to 200).map { i => new Account(i.toString, 1) }
  val account = new Account("dest", 0)
  val t1 = thread {
    sendAll(accounts1.toSet, account)
  }
  val t2 = thread {
    sendAll(accounts2.toSet, account)
  }
  t1.join()
  t2.join()
  println(s"dest balance = ${account.money}")
  println(s"all other accounts empty = ${(accounts1 ++ accounts2).forall(_.money == 0)}")
}

object SynchronizedPriorityPool extends App {
  val pool = new PriorityTaskPool(concurrency, concurrency / 2)
  (1 to 100).map { i =>
    pool.asynchronous(Random.nextInt(concurrency)) {
      Thread.sleep(1)
      println(i)
    }
  }
  pool.shutdown()
}

object ConcurrentBidiMapTest extends App {
  private def toExcelColumnName(i: Int) = {
    val letter = ('A' + (i - 1) % 26).toChar
    letter.toString * ((i - 1) / 26 + 1)
  }

  val cbm = new ConcurrentBiMap[Int, String]
  val range = 1 to 20000000

  private def startAddThread() = {
    thread {
      range.iterator.foreach { i => cbm.put(i, i.toString) }
    }
  }

  val threads = List.fill(concurrency)(startAddThread())
  threads.foreach(_.join())
  println(cbm.size)
}

object MultithreadedMemoize extends App {
  val range = 1 to 1500

  // stack overflows...
  def fibonacci(n: BigInt): BigInt = {
    if (n == 1) 1
    else if (n == 2) 1
    else cacheFib(n - 1) + cacheFib(n - 2)
  }

  lazy val cacheFib = cache(fibonacci)

  def fibThread(i: Int) = thread {
    range.map(_ * concurrency - i).iterator.map(cacheFib.compose(BigInt.apply))
  }

  val threads = (0 until concurrency).map(fibThread)
  threads.foreach(_.join())
  println(s"the ${range.`end`}th fib num is ${cacheFib(range.`end`)}")
}
