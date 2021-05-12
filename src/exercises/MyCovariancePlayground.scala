package exercises

object MyCovariancePlayground extends App {

  class Vehicle
  class Bike extends Vehicle
  class Car extends Vehicle

  // Invariant
  class InvariantParking[T](things: List[T]) {
    def park(vehicle: T): InvariantParking[T] = this
    def impound(vehicles: List[T]): InvariantParking[T] = this
    def checkVehicles(conditions: String): List[T] = List()
  }

  // implementation of this/List is just to make it pass.
  // covariant
  class CovariantParking[+T](things: List[T]) {
    def park[S >: T](vehicle: S): CovariantParking[S] = this // [S >: T] "widens"
    def impound[S >: T](vehicles: List[S]): CovariantParking[S] = this
    def checkVehicles(  conditions: String): List[T] = List()
  }

  class ContravariantParking[-T](things: List[T]) {
    def park(vehicle: T): ContravariantParking[T] = this
    def impound[T](vehicles: List[T]): ContravariantParking[T] = new ContravariantParking[T](List())
    def checkVehicles[S <: T](conditions: String): List[S] = List()
  }

}
