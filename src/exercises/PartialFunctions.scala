/*
 * Copyright (c) 2021 Who. All rights reserved.
 */
package exercises

object PartialFunctions extends App {

  val myPartialFunc = new PartialFunction[Int, Int] {
    override def isDefinedAt(x: Int): Boolean = Set(1, 5).contains(x)

    override def apply(v1: Int): Int = v1 match {
      case 1 => 5
      case 5 => 7
    }
  }

  println(myPartialFunc(5))
  println(myPartialFunc.lift(8).getOrElse(9))

  /* Chat Bot with Partial Functions */
  // scala.io.Source.stdin.getLines().foreach(line => println("you said: " + line))
  List("whats up", "not me").map{
    case "me" => "you"
    case "not me" => "not you"
    case "whats up" => "what u said"
  }.foreach(println)
}
