package parking.vehicle

case object VehicleType extends Enumeration {
  type VehicleType = Value
  val electric, van = Value
  val petrolDiesel: VehicleType.Value = Value("petrol/diesel")
}
