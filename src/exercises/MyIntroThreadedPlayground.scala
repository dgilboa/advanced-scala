/*
 * Copyright (c) 2021 Who. All rights reserved.
 */
package exercises

object MyIntroThreadedPlayground extends App {

  // Let's write the inception threads
  def inceptionThread(id: Int, upTo: Int): Thread = {
    if (id == upTo) {
      new Thread(() => println(s"Thread $id"))
    } else {
      val next = inceptionThread(id + 1, upTo)
      next.start()
      next.join()
      new Thread(() => println(s"Thread $id"))
    }
  }

  inceptionThread(1, 50).start()

  /**  Actually their implementation is nicer @see [[lectures.part3concurrency.Intro.inceptionThreads()]] */
}