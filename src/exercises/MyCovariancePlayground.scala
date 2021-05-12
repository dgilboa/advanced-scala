package exercises

object MyCovariancePlayground extends App {

  class Vehicle
  class Bike extends Vehicle
  class Car extends Vehicle

  // Invariant
  class InvariantParking[T](things: List[T]) {
    def park(vehicle: T): InvariantParking[T] = ???
    def impound(vehicles: List[T]): InvariantParking[T] = ???
    def checkVehicles(conditions: String): List[T] = ???
  }

  // covariant
  class CovariantParking[+T](things: List[T]) {
    def park[S >: T](vehicle: S): CovariantParking[S] // [S >: T] "widens"
    def impound[S >: T](vehicles: List[S]): CovariantParking[S]
    def checkVehicles(  conditions: String): List[T]
  }

  class ContravariantParking[-T](things: List[T]) {
    def park(vehicle: T): ContravariantParking[T]
    def impound[T](vehicles: List[T]): ContravariantParking[T]
    def checkVehicles[S <: T](conditions: String): List[S]
  }

}
