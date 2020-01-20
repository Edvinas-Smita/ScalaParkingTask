import org.scalatest.Matchers._
import org.scalatest._
import parking.parkingLot.ParkingLot
import parking.vehicle.{VehicleInfo, VehicleType}

class GarageFloorTest extends FunSuite {
  test("ParkingLot.ParkingLot.bestFloor - one floor fits all") {
    val oneFloor = ParkingLot((0, 0), 3)
    val petrol = VehicleInfo.defaultVehicleFloorRestrictions(VehicleInfo("", VehicleType.petrolDiesel))
    val electric = VehicleInfo.defaultVehicleFloorRestrictions(VehicleInfo("", VehicleType.electric))
    val van = VehicleInfo.defaultVehicleFloorRestrictions(VehicleInfo("", VehicleType.van))

    val petOpt = oneFloor.nearestFloor(0, petrol)
    petOpt should be(Some(0))
    assert(oneFloor.parkAtFloor(0))

    val elOpt = oneFloor.nearestFloor(0, electric)
    elOpt should be(Some(0))
    assert(oneFloor.parkAtFloor(0))

    val vanOpt = oneFloor.nearestFloor(0, van)
    vanOpt should be(Some(0))
    assert(oneFloor.parkAtFloor(0))

    oneFloor.nearestFloor(0, petrol) should be(None)
    assert(!oneFloor.parkAtFloor(0))
  }

  test("ParkingLot.ParkingLot.bestFloor - distance grows + petrol any floor") {
    val garage = ParkingLot((0, 2), 1)
    val petrol = VehicleInfo.defaultVehicleFloorRestrictions(VehicleInfo("", VehicleType.petrolDiesel))

    //entering from the very bottom with capacity of 1 means the nearest floors are always increasing by 1
    //petrol should fit into all 3 floors

    val petOpt1 = garage.nearestFloor(0, petrol)
    petOpt1 should be(Some(0))
    assert(garage.parkAtFloor(0))

    val petOpt2 = garage.nearestFloor(0, petrol)
    petOpt2 should be(Some(1))
    assert(garage.parkAtFloor(1))

    val petOpt3 = garage.nearestFloor(0, petrol)
    petOpt3 should be(Some(2))
    assert(garage.parkAtFloor(2))

    garage.nearestFloor(0, petrol) should be(None)
  }

  test("ParkingLot.ParkingLot.bestFloor - electric top floors") {
    val garage = ParkingLot((0, 2), 1)
    val electric = VehicleInfo.defaultVehicleFloorRestrictions(VehicleInfo("", VehicleType.electric))

    val elOpt1 = garage.nearestFloor(0, electric)
    elOpt1 should be(Some(1))
    assert(garage.parkAtFloor(1))

    val elOpt2 = garage.nearestFloor(0, electric)
    elOpt2 should be(Some(2))
    assert(garage.parkAtFloor(2))

    // Top 2 floors are full so there is no space for electric
    val elOpt3 = garage.nearestFloor(0, electric)
    elOpt3 should be(None)
  }

  test("ParkingLot.ParkingLot.bestFloor - van bottom floors") {
    val garage = ParkingLot((0, 2), 1)
    val van = VehicleInfo.defaultVehicleFloorRestrictions(VehicleInfo("", VehicleType.van))

    val vanOpt1 = garage.nearestFloor(0, van)
    vanOpt1 should be(Some(0))
    assert(garage.parkAtFloor(0))

    val vanOpt2 = garage.nearestFloor(0, van)
    vanOpt2 should be(Some(1))
    assert(garage.parkAtFloor(1))

    // Bottom 2 floors are full so there is no space for vans
    val vanOpt3 = garage.nearestFloor(0, van)
    vanOpt3 should be(None)
  }

  test("ParkingLot.ParkingLot.bestFloor - only negative floors") {
    val garage = ParkingLot((-2, -1), 1)
    val petrol = VehicleInfo.defaultVehicleFloorRestrictions(VehicleInfo("", VehicleType.petrolDiesel))

    garage.nearestFloor(0, petrol) should be(None) //entering at non-existent floor

    val petOpt1 = garage.nearestFloor(-1, petrol)
    petOpt1 should be(Some(-1))
    assert(garage.parkAtFloor(-1))

    val petOpt2 = garage.nearestFloor(-1, petrol)
    petOpt2 should be(Some(-2))
    assert(garage.parkAtFloor(-2))

    garage.nearestFloor(-1, petrol) should be(None)
  }
}
