/*
 * Copyright (c) 2021 Lightricks. All rights reserved.
 */
package exercises

import scala.collection.mutable
import scala.util.Random

object MyThreadsPlayground extends App {

  val myBasicThread = new Thread(new Runnable {
    override def run(): Unit = println("my Basic Thread")
  })

  //  myBasicThread.start()

  def myMultiThreadedRun(nConsumers: Int, nProducers: Int, capacity: Int) = {
    val aBuffer = new mutable.Queue[Int]
    for (consumerId <- 1 to nConsumers) new MyConsumer(consumerId, aBuffer).start()
    for (producerId <- 1 to nProducers) new MyProducer(producerId, aBuffer, capacity).start()
  }

  /** multi threaded producer-consumer */

  class MyConsumer(id: Int, buffer: mutable.Queue[Int]) extends Thread {
    val rand = new Random()

    override def run(): Unit = {
      print("starting")
      while (true) {
        buffer.synchronized {
          if (buffer.isEmpty) {
            print("buffer empty - waiting")
            buffer.wait()
          }

          // Since now I know can take and it's not empty
          val next = buffer.dequeue()
          print(s"pulled $next")
          buffer.notify()

        }
        Thread.sleep(rand.nextInt(500))
      }
    }

    def print(msg: String) = println(s"[Consumer $id] - $msg")

  }

  class MyProducer(id: Int, buffer: mutable.Queue[Int], capacity: Int) extends Thread {
    val rand = new Random()
    var i = 0

    override def run(): Unit = {
      print("starting")
      while (true) {
        buffer.synchronized {
          if (buffer.size >= capacity) {
            print("buffer full - blocking")
            buffer.wait()
          }

          // Since now I know can push more and it's not full
          buffer.enqueue(i)
          print(s"pushed $i")
          i += 1

          buffer.notify()
        }
        Thread.sleep(rand.nextInt(250))
      }
    }

    def print(msg: String) = println(s"[Producer $id] - $msg")
  }

  /**  No this is only for one producer and one consumer - works fine. */
//  myMultiThreadedRun(1, 1, 10)

  /** When this runs with the existing implementation some threads crash due to
    * deque from empty queue  */
//  myMultiThreadedRun(3, 3, 10)

  /** Now for real working multithreaded */
  class MyMultiThreadedConsumer(id: Int, buffer: mutable.Queue[Int]) extends Thread {
    val rand = new Random()

    override def run(): Unit = {
      print("starting")
      while (true) {
        buffer.synchronized {
          // now it's not a simple if but while as one of the other consumers can take it
          while (buffer.isEmpty) {
            print("buffer empty - waiting")
            buffer.wait()
          }

          // Since now I know can take and it's not empty
          val next = buffer.dequeue()
          print(s"pulled $next")
          buffer.notify()

        }
        Thread.sleep(rand.nextInt(500))
      }
    }

    def print(msg: String) = println(s"[Consumer $id] - $msg")

  }

  class MyMultiThreadedProducer(id: Int, buffer: mutable.Queue[Int], capacity: Int) extends Thread {
    val rand = new Random()
    var i = 0

    override def run(): Unit = {
      print("starting")
      while (true) {
        buffer.synchronized {
          /** while and not if - see the comment in consumer */
          while (buffer.size >= capacity) {
            print("buffer full - blocking")
            buffer.wait()
          }

          // Since now I know can push more and it's not full
          buffer.enqueue(i)
          print(s"pushed $i")
          i += 1

          buffer.notify()
        }
        Thread.sleep(rand.nextInt(250))
      }
    }

    def print(msg: String) = println(s"[Producer $id] - $msg")
  }

  def myRealMultiThreadedRun(nConsumers: Int, nProducers: Int, capacity: Int) = {
    val aBuffer = new mutable.Queue[Int]
    for (consumerId <- 1 to nConsumers) new MyMultiThreadedConsumer(consumerId, aBuffer).start()
    for (producerId <- 1 to nProducers) new MyMultiThreadedProducer(producerId, aBuffer, capacity).start()
  }
  myRealMultiThreadedRun(3, 3, 10)

}
