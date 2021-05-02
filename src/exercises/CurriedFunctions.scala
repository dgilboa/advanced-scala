package exercises

/** Solution for exercises @see [[lectures.part2afp.CurriesPAF]] */
object CurriedFunctions {

  /** Exe 1 - curry method, function, curried function */
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x+ y
  def curriedAddMethod(x: Int)(y: Int) = x + y

//  val add7: Int => Int = y => 7 + y
  val add7: Int => Int = curriedAddMethod(7)
  val add_7 = curriedAddMethod(7) _ // he explained it but not sure I got it.
  def add_a7(x: Int): Int = simpleAddFunction(x, 7)
  val add_another7 =  (x: Int)  => simpleAddFunction(x, 7)


}
