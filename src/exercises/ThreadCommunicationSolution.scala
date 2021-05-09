package exercises

object ThreadCommunicationSolution extends App {

  /*
  Exercises.
  1) think of an example where notifyALL acts in a different way than notify?
  2) create a deadlock
  3) create a livelock
 */

  // notifyAll.
  def whereNotifyAllRelevant() = {
    val holdMe = new Object

    (1 to 10).foreach(i => new Thread(() => {
      holdMe.synchronized {
        println(f"[Thread $i%2d] waiting")
        holdMe.wait()
        println(s"[Thread $i] got it")
      }
    }).start())

//    ('a' to 'o').foreach(i => new Thread(() => {
//      holdMe.synchronized {
//        println(s"[Thread $i] yo yo")
//        holdMe.notifyAll()
//        println(s"[Thread $i] bye bye ")
//      }
//    }).start())

    new Thread(() => {
      Thread.sleep(2500)
      println("[Server] I'm here !")
      holdMe.synchronized {
        // with only notify - only one Thread will get it back and the rest will block
        holdMe.notify()
//        holdMe.notifyAll()
      }
    }).start()
  }

  // commented out so we can run the other things.
//  whereNotifyAllRelevant()

  // deadlock - both threads are waiting for each other.
  def deadlockExample(): Unit = {
    val lock = new Object

    new Thread(() => {
      lock.synchronized {
        println("[Thread A] starting")
        lock.wait()
        println("[Thread A] done")
      }
    }).start()

    new Thread(() => {
      lock.synchronized {
        println("[Thread B] starting")
        lock.wait()
        println("[Thread B] done")
      }
    }).start()
  }

//  deadlockExample()

  // livelock - Both threads are "working" (not blocked) but can't progress.

  // just playing
  case class Message(saywht: String){
    def show(): Unit = println(s"showing $saywht")
  }

  val myMsg = Message("What What")
  println(s"$myMsg")
  myMsg.show()
}
