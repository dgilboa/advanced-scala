package exercises

object ImplicitPlayground extends App {

  /** First exercise */

  case class Person(name: String, age: Int){
    override def toString: String = s"Hi There, My name is $name and I'm $age yo."
  }

  implicit val ageBasedPersonOrdering: Ordering[Person] = Ordering.fromLessThan(_.age > _.age)
  implicit val lexNameBasedPersonOrdering: Ordering[Person] = Ordering.fromLessThan(_.name < _.name)

  val people =
    List(Person("Dror", 38), Person("Daniella", 36), Person("Harel", 4), Person("Sahar", 2))

//  println(people.mkString("\n"))
  println("Ordered people by age:\n" + people.sorted(ageBasedPersonOrdering).mkString("\n"))
  println("Ordered people by name:\n" + people.sorted(lexNameBasedPersonOrdering).mkString("\n"))

}
