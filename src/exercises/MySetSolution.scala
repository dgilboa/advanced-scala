/*
 * Copyright (c) 2021 Lightricks. All rights reserved.
 */
package exercises

import scala.annotation.tailrec

/** Declaring my own set to not interfere with the given code */
trait MyOwnSet[A] extends (A => Boolean) {

  /** This applies to all the sub types of this set */
  def apply(elem: A): Boolean = contains(elem)

  def contains(elem: A): Boolean
  def +(elem: A): MyOwnSet[A]
  def ++(anotherSet: MyOwnSet[A]): MyOwnSet[A]
  
  def map[B](f: A => B): MyOwnSet[B]
  def flatMap[B](f: A => MyOwnSet[B]): MyOwnSet[B]
  def filter(predicate: A => Boolean): MyOwnSet[A]
  def foreach(f: A => Unit): Unit
  
}

class MyOwnEmptySet[A] extends MyOwnSet[A] {
  override def contains(elem: A): Boolean = false
  override def +(elem: A): MyOwnSet[A] = new MyOwnNonEmptySet[A](elem, this)
  override def ++(anotherSet: MyOwnSet[A]): MyOwnSet[A] = anotherSet

  override def map[B](f: A => B): MyOwnSet[B] = new MyOwnEmptySet[B]
  override def flatMap[B](f: A => MyOwnSet[B]): MyOwnSet[B] = new MyOwnEmptySet[B]
  override def filter(predicate: A => Boolean): MyOwnSet[A] = this
  override def foreach(f: A => Unit): Unit = ()

}

class MyOwnNonEmptySet[A](val value: A, val next: MyOwnSet[A]) extends MyOwnSet[A] {

  override def contains(elem: A): Boolean = elem == value || next.contains(elem)
  override def +(elem: A): MyOwnSet[A] =
    if (this contains elem) this
    else new MyOwnNonEmptySet[A](elem, this)

  override def ++(anotherSet: MyOwnSet[A]): MyOwnSet[A] = {
    if (anotherSet.contains(value)) next ++ anotherSet
    else new MyOwnNonEmptySet[A](value, next ++ anotherSet)
  }
  /**  his solution with nice explanation
    * tail ++ anotherSet + head
    * */

  override def map[B](f: A => B): MyOwnSet[B] = new MyOwnNonEmptySet[B](f(value), next.map(f))
  override def flatMap[B](f: A => MyOwnSet[B]): MyOwnSet[B] = {
    val mapped = f(value)
    mapped match {
      case _:MyOwnEmptySet[B] => next.flatMap(f)
      case nonEmpty: MyOwnNonEmptySet[B] => nonEmpty ++ next.flatMap(f)
    }
    // all of this looks equal to this : mapped ++ next.flatMap(f)
  }
  override def filter(predicate: A => Boolean): MyOwnSet[A] =
    if (predicate(value)) new MyOwnNonEmptySet[A](value, next.filter(predicate))
    else next.filter(predicate)

  override def foreach(f: A => Unit): Unit = {
    f(value)
    next.foreach(f)
  }
}

object MyOwnSet {
  def apply[A](elements : A*): MyOwnSet[A] = {
    @tailrec
    def buildMyOwnSet(values: Seq[A], acc: MyOwnSet[A]): MyOwnSet[A] = {
      if (values.isEmpty) acc
      else buildMyOwnSet(values.tail, acc + values.head)
    }
    buildMyOwnSet(elements.toSeq, new MyOwnEmptySet[A])
  }
}

object MySetSolutionPlayground extends App {
  val mySet = MyOwnSet[Int](1, 5, 6, 5)
  println(mySet)
  println("Is 10 in my set: " +mySet(10))
  println("Is 5 in my set: " +mySet(5))
  mySet foreach println

}
