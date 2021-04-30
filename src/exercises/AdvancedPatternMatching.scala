/*
 * Copyright (c) 2021 Lightricks. All rights reserved.
 */
package exercises

import exercises.AdvancedPatternMatching.{Digit, Even}

/** Solution for the exercises in the the lecture in @see [[lectures.part1as.AdvancedPatternMatching]] */
object AdvancedPatternMatching extends App {

  object Even {
    /** we cant define more than one unapply */
//    def unapply(x: Int): Option[String] =
//      if (x % 2 == 0) Some("I am an even number") else None
    def unapply(x: Int): Boolean = x % 2 == 0
  }

  object Digit {
//    def unapply(x: Int): Option[String] =
//      if (x < 10 && x >= 0) Some("I am a digit") else None
    def unapply(x: Int): Boolean = x < 10 && x >= 0
  }

  val n: Int = 45
  val mathProperty: String = n match {
    case x: Int if x.<(10) => "single digit"
    case x: Int if x.%(2).==(0) => "an even number"
    case _ => "no Property"
  }

  val mathPropertyAdvanced: String = n match {
    case Digit() => "single digit"
    case Even() => "an even number"
    case _ => "no Property"
  }

}
