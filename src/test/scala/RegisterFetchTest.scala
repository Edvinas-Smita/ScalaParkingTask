import java.io.File

import org.scalatest.FunSuite
import parking.vehicle.VehicleInfo

class RegisterFetchTest extends FunSuite {
  test("Vehicle.VehicleInfo.fetchInfo") {
    val fileName = s".${File.separator}VehicleRegister.xml"
    assert(VehicleInfo.fetchInfoXML(fileName, "ABC123").nonEmpty) //Correct input, correct data => should return Some
    assert(VehicleInfo.fetchInfoXML(fileName, "BCD123").isEmpty) //Correct input, bad data => should return None
    assert(VehicleInfo.fetchInfoXML(fileName, "").isEmpty) //Incorrect input => should return None
    assert(VehicleInfo.fetchInfoXML(fileName, "BCD234").isEmpty) //Correct input, one datum is correct, but it comes after an incorrect datum
  }
}
