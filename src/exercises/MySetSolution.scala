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
  def ++(anotherSet: MyOwnSet[A]): MyOwnSet[A] // union
  
  def map[B](f: A => B): MyOwnSet[B]
  def flatMap[B](f: A => MyOwnSet[B]): MyOwnSet[B]
  def filter(predicate: A => Boolean): MyOwnSet[A]
  def foreach(f: A => Unit): Unit

  def -(elem: A): MyOwnSet[A]
  def intersection(anotherSet: MyOwnSet[A]): MyOwnSet[A] // &
  def diff(anotherSet: MyOwnSet[A]): MyOwnSet[A] // --

  def unary_! : MyOwnSet[A] // to be used as !mysetinstance --> that's actually just a syntactic suger.
}

class MyOwnEmptySet[A] extends MyOwnSet[A] {
  override def contains(elem: A): Boolean = false
  override def +(elem: A): MyOwnSet[A] = new MyOwnNonEmptySet[A](elem, this)
  override def ++(anotherSet: MyOwnSet[A]): MyOwnSet[A] = anotherSet

  override def map[B](f: A => B): MyOwnSet[B] = new MyOwnEmptySet[B]
  override def flatMap[B](f: A => MyOwnSet[B]): MyOwnSet[B] = new MyOwnEmptySet[B]
  override def filter(predicate: A => Boolean): MyOwnSet[A] = this
  override def foreach(f: A => Unit): Unit = ()

  override def -(elem: A): MyOwnSet[A] = this
  override def intersection(anotherSet: MyOwnSet[A]): MyOwnSet[A] = this
  override def diff(anotherSet: MyOwnSet[A]): MyOwnSet[A] = this

  /** Cause for the empty set - the negative to it is all true */
  override def unary_! : MyOwnSet[A] = new MyOwnPropertyBasedSet[A](_ => true)
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

  override def -(elem: A): MyOwnSet[A] =
    if (value == elem) next
    else next - elem + value

  override def intersection(anotherSet: MyOwnSet[A]): MyOwnSet[A] = {
    filter (e => anotherSet.contains(e))
    // filter(e => anotherSet(e)) equals
    // filter(anotherSet) // intersection = filtering !!
  }

  override def diff(anotherSet: MyOwnSet[A]): MyOwnSet[A] = {
    filter (e => !anotherSet.contains(e))
    // filter(!anotherSet)
  }

  override def unary_! : MyOwnSet[A] = ???
}

class MyOwnPropertyBasedSet[A](property: A => Boolean) extends MyOwnSet[A] {

  override def contains(elem: A): Boolean = property(elem)

  override def +(elem: A): MyOwnSet[A] =
    new MyOwnPropertyBasedSet[A](x => property(x) || x == elem)

  override def ++(anotherSet: MyOwnSet[A]): MyOwnSet[A] =
    new MyOwnPropertyBasedSet[A](x => property(x) || anotherSet(x))

  // goes for all A..
  // if predicate(x) than f(x) but where do I get x:A
  override def map[B](f: A => B): MyOwnSet[B] = politelyFail
  override def flatMap[B](f: A => MyOwnSet[B]): MyOwnSet[B] = politelyFail

  // should hold first this predicate and than the other predicate
  override def filter(predicate: A => Boolean): MyOwnSet[A] =
    new MyOwnPropertyBasedSet[A](x => property(x) && predicate(x))

  // should go over all A
  override def foreach(f: A => Unit): Unit = politelyFail

  override def -(elem: A): MyOwnSet[A] =
    new MyOwnPropertyBasedSet[A](x => property(x) && x != elem)
    // actually also: filter(x => x != elem)

  override def intersection(anotherSet: MyOwnSet[A]): MyOwnSet[A] =
    new MyOwnPropertyBasedSet[A](x => property(x) && anotherSet(x))
    // actually also: filter(anotherSet)

  override def diff(anotherSet: MyOwnSet[A]): MyOwnSet[A] = {
    new MyOwnPropertyBasedSet[A](x => property(x) && !anotherSet(x))
    // actually also: filter(!anotherSet)
  }

  override def unary_! : MyOwnSet[A] = new MyOwnPropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Not Supported")
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

  println(mySet - 5 contains(5))

}
