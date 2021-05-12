package exercises

object MyJsonSerializationPlayground extends App {
  println("Json Serialization")

  sealed trait JValue {
    def stringify: String
  }

  final case class JString(value: String) extends JValue {
    override def stringify: String = "\"" + value + "\""
  }

  final case class JNumber(value: Int) extends JValue {
    override def stringify: String = value.toString
  }

  final case class JArray(values: List[JValue]) extends JValue {
    override def stringify: String = values.map(_.stringify).mkString("[",",","]")
  }

  final case class JObject(fields: Map[String, JValue]) extends JValue {
//    override def stringify: String = {
//      "{" + fields.map(field => field._1 + ":" + field._2.stringify).mkString(",") + "}"
//    }

    override def stringify: String = fields.map {
      case (name, value) => "\"" + name + "\":" + value.stringify
    }.mkString("{", ",", "}")
  }

  // Direct JValue
  val personJson = JObject(Map(
    "name" -> JString("Sam"),
    "age" -> JNumber(10),
    "etc" -> JArray(List(JString("kind"), JNumber(42)))))
  println(personJson.stringify)

  // Type class for converting to JsonValue.
  trait JsonConv[T] {
    def convert(value: T): JValue
  }

//  println(StringConv.convert("hello").stringify)

  implicit object StringConv extends JsonConv[String]{
    override def convert(value: String): JValue = JString(value)
  }

  implicit object IntConv extends JsonConv[Int]{
    override def convert(value: Int): JValue = JNumber(value)
  }

  implicit object EtcConv extends JsonConv[Etc]{
    override def convert(value: Etc): JValue = JObject(Map(
      "just" -> JString(value.just)
    ))
  }

  implicit object PersonConv extends JsonConv[Person]{
    override def convert(value: Person): JValue = JObject(Map(
      "name" -> value.name.toJson,
      "age" -> value.age.toJson,
      "etcs" -> JArray(value.etcs.map(_.toJson))
    ))

    // another option :
//    override def convert(value: Person): JValue = JObject(Map(
//      "name" -> JString(value.name),
//      "age" -> JNumber(value.age),
//      "etcs" -> JArray(value.etcs.map(EtcConv.convert))
//    ))
  }

  // Conversion ops helper
  implicit class JsonConvOps[T](value: T) {
     def toJson(implicit convertor: JsonConv[T]): JValue = convertor.convert(value)

//    applying on this is actually for instantiating a TypeClassTemplate for case we have more than one def.
//    def apply()
  }


  case class Etc(just: String)
  case class Person(name: String, age: Int, etcs: List[Etc])
  val aPerson = Person("Someone", 40, List(Etc("guess"), Etc("who")))
  println(aPerson.toString + " ::: " + aPerson.toJson.stringify)


}
