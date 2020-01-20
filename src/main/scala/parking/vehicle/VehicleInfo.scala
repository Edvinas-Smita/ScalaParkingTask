package parking.vehicle

import java.util.NoSuchElementException

import parking.vehicle.VehicleType.VehicleType

import scala.xml.XML

case class VehicleInfo(licencePlate: String, vehicleType: VehicleType)

case object VehicleInfo {
  def fetchInfoXML(fileName: String, licencePlate: String): Option[VehicleInfo] = {
    val xml = XML.loadFile(fileName)

    // Select all vehicle elements
    (xml \\ "vehicle")
      // then find the first element with matching plate attribute
      .find(n => n \@ "plate" == licencePlate)
      // flatMap takes care of None from find
      .flatMap(vehicle => {
        try {
          // Convert <type> text node value into enum
          val vehicleType = VehicleType withName (vehicle \ "type").text
          Some(VehicleInfo(licencePlate, vehicleType))
        } catch {
          // If there is no enum with such name.
          // Catches 2 in 1, because if no <type> element exists, .text returns an empty string
          case _: NoSuchElementException => None
        }
      })
  }

  def defaultVehicleFloorRestrictions(vehicleInfo: VehicleInfo): (Int, Int) => Boolean = {
    (totalFloors: Int, testFloor: Int) => {
      vehicleInfo.vehicleType match {
        case VehicleType.electric => totalFloors - testFloor <= 2 // Upper 2 floors for electric
        case VehicleType.van => testFloor < 2 // Lower 2 floors for vans
        case VehicleType.petrolDiesel => true // All floors for petrol/diesel
      }
    }
  }
}