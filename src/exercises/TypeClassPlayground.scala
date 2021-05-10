package exercises

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
//  println("My new type class Equality :" + Equality[Person].isEqual(sam, otherSam))

  println("My new type class Equality :" + Equality[Person](sam, sam))
  println("My new type class Equality :" + Equality[Person](sam, bob))

  println("My new type class Equality :" + Equality.apply(sam, sam))
  println("My new type class Equality :" + Equality.apply(sam, bob))

  // AD-HOC polymorphism */
  println("My new type class Equality :" + Equality(sam, sam))
  println("My new type class Equality :" + Equality(sam, bob))
}
