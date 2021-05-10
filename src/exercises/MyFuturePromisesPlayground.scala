package exercises

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object MyFuturePromisesPlayground extends App {

  val justSomething: Int = 42
  // note that no new Future() as we're using the apply in the Future Companion object.
  val aFuture = Future {
    justSomething
  }

  aFuture.onComplete {
    case Success(result) => println("got a result ")
    case Failure(exception) => println("failed !" + exception)
  }

  println("My Value : " + aFuture.value)

}
