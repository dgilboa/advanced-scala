package exercises
import java.util.Optional

object ScalaJavaConvPlayground extends App {

  /** Scala to Java Optional to Option */
  trait ScalaFromJava[FromJava, ToScala] {
    def toScala(value: FromJava): ToScala
  }

  implicit object OptionalToOption extends ScalaFromJava[java.util.Optional[Int], Option[Int]]{
    override def toScala(value: Optional[Int]): Option[Int] =
      if (value.isPresent) Some(value.get())
      else None
  }

  implicit class ScalaOps[FromJava, ToScala](value: FromJava) {
    def asScala(implicit convertor: ScalaFromJava[FromJava, ToScala]): ToScala = convertor.toScala(value)
  }

  val jOptional = java.util.Optional.of[Int](42)
  println(jOptional)
  println(jOptional.asScala)

  // now with his idea
  class ToScala[T](value: => T) {
    def asScala1: T = value
  }

  implicit def asScalaOptional[T](from: Optional[T]): ToScala[Option[T]] =
    new ToScala[Option[T]](if (from.isPresent) Some(from.get()) else None)

  val jOptional1 = Optional.of(2)
  println(jOptional1.asScala1)

}
