package exercises

import scala.annotation.tailrec

object TypeClassPlayground extends App {

  case class Person(name: String, age: Int)

  // Equal type class
  trait Equal[T] {
    def apply(a:T, b: T): Boolean
  }

  object PersonEqual extends Equal[Person] {
    override def apply(a: Person, b: Person): Boolean = a.equals(b)
  }

  val sam = Person("sam", 10)
  val bob = Person("bob", 10)
  val otherSam = Person("sam", 10)

  println(PersonEqual.apply(sam, bob) + " == " + PersonEqual(sam, bob))

  // true cause it's a case class.
  println(PersonEqual.apply(sam, otherSam) + " == " + PersonEqual(sam, otherSam))

  // Type Class Templating //
  trait MyTypeClassTemplate[T] {
    def action(a: T): Unit
  }

  object MyTypeClassTemplate {
    def apply[T](implicit instance: MyTypeClassTemplate[T]) = instance
  }

  // my new equality
  trait Equality[T] {
    def isEqual(a: T, b: T): Boolean
    def apply(a: T, b: T): Boolean
  }

  object Equality {
//    def apply[T](implicit instance: Equality[T]) = instance
//    def apply[T](a: T, b: T)(implicit equalizer: Equality[T]) = equalizer.isEqual(a, b)
    def apply[T](a: T, b: T)(implicit equalizer: Equality[T]) = equalizer.apply(a, b)

    implicit object PersonEquality extends Equality[Person] {
      override def isEqual(a: Person, b: Person): Boolean = a == b
      override def apply(a: Person, b: Person): Boolean = isEqual(a, b)
    }
  }

//  println("My new type class Equality :" + Equality[Person].isEqual(sam, sam))
//  println("My new type class Equality :" + Equality[Person].isEqual(sam, bob))
//  println("My new type classes Equality :" + Equality[Person].isEqual(sam, otherSam))

  println("My new type class Equality :" + Equality[Person](sam, sam))
  println("My new type class Equality :" + Equality[Person](sam, bob))

  println("My new type class Equality :" + Equality.apply(sam, sam))
  println("My new type class Equality :" + Equality.apply(sam, bob))

// AD-HOC polymorphism */
  println("My new type class Equality :" + Equality(sam, sam))
  println("My new type class Equality :" + Equality(sam, bob))

  /** Pimp my library exec */

  implicit class RichString(val value: String) extends AnyVal {
    def asInt: Int = value.toInt
    def asAnotherInt: Int = Integer.valueOf(value)

    def encrypt: String = value.reverse

  }

  println("12".asInt)
//  println("John".asInt) // raise error !
  println("John".encrypt)

  implicit class RichInt(val value: Int) extends AnyVal {
    def times(f: () => Unit): Unit = {
      require(value >= 0) // to be explored a bit more with refined
      def timesDo(n: Int): Unit =
        if (n <= 0) ()
        else {
          f()
          timesDo(n - 1)
        }
      timesDo(value)
    }

    def *(list: List[Int]): List[Int] = {
      @tailrec
      def con(n: Int, acc: List[Int]): List[Int] =
        if (n <= 0 ) acc
        else
          con(n -1, acc ++ list)

      con(value, List.empty)
    }

    // Just a ismpler version of *
    def **[T](list: List[T]): List[T] = {
      def con(n: Int): List[T] =
        if (n <= 0 ) Nil
        else con(n - 1) ++ list

      con(value)
    }

  }

  3.times(() => print(" Hi "))
  0.times(() => print(" Bye "))

  println()
  println(3.*(List(1, 2)))
  println(3 * List(1, 2))
  println(3.**(List(1, 2)))
  println(3 ** List(1, 2))

  // Pimp My Library Equality as enrichment class
  trait Equality2[T] {
    def ===(anotherValue: T): Boolean
    def !==(anotherValue: T): Boolean = !(===(anotherValue))
  }

  implicit class PersonEquality2(me: Person) extends Equality2[Person] {
    override def ===(anotherValue: Person): Boolean =
      me.name.toLowerCase == anotherValue.name.toLowerCase && me.age == anotherValue.age
  }

  implicit class Equality2Enricher[T](value: T) {
    def ===(anotherValue: T)(implicit equalizer: Equality2[T]): Boolean = this === anotherValue
  }

  val samUpperCase = Person("SAM", 10 )
  println("Using Enriched Type Class : "+ sam.===(bob))
  println("Using Enriched Type Class : "+ sam.===(samUpperCase))

  /**
    * So for Type Classes Support we need :
    *   - type class itself (a trait with the desired behaviour)
    *   - type class instances (implementation of the type class for specific types, usually as implicits)
    *   - "conversion with implicit classes" - probably the OPS part - this does the magic of enrichment.
    */

  // implicitly
  case class Permission(mask: String)
  implicit val defaultPermission = Permission("0744")

  val standardPerms = implicitly[Permission] // ==> implicitly[T](e: T): T = e
  println(standardPerms)

}
