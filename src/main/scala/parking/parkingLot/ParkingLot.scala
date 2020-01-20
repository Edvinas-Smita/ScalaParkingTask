package parking.parkingLot

import scala.xml.XML

class ParkingLot private(garage: Array[Int], lowestFloor: Int) {
  // Redundant: validation is done in companion object
  if (garage == null) {
    throw new IllegalArgumentException("garage cannot be null")
  }

  def nearestFloor(entryFloor: Int, floorValidation: (Int, Int) => Boolean): Option[Int] = {
    if (!isValidFloor(entryFloor)) {
      None
    } else {
      val entryIdx = entryFloor - lowestFloor
      try {
        Some(
          garage
            .zipWithIndex // Zip to (Free spaces, floor index in garage)
            .filter(zip => canParkAtFloor(zip._2 + lowestFloor, floorValidation)) // Choose available floors
            .map(zip => (Math.abs(zip._2 - entryIdx), zip._2)) // Map to (distances from entry, floor index)
            .min(Ordering.by[(Int, Int), Int](_._1)) // Find min distance
            ._2 + lowestFloor
        )
      } catch {
        case _: UnsupportedOperationException => None // If min gets nothing
      }
    }
  }

  /**
   *
   * @param floor           floor to test
   * @param floorValidation predicate for parking at specific floor in garage: (total garage height, test floor)
   * @return
   */
  def canParkAtFloor(floor: Int, floorValidation: (Int, Int) => Boolean): Boolean = {
    val floorIdx = floor - lowestFloor
    isValidFloor(floor) && // Floors that exist,...
      floorHasSpace(floor) && // ...are not full...
      floorValidation(garage.length, floorIdx) // ...and match vehicle type criteria
  }

  def floorHasSpace(floor: Int): Boolean = garage(floor - lowestFloor) > 0

  def isValidFloor(floor: Int): Boolean = {
    val entryIdx = floor - lowestFloor
    !(entryIdx < 0 || entryIdx >= garage.length)
  }

  def parkAtFloor(floor: Int): Boolean = {
    if (isValidFloor(floor) && floorHasSpace(floor)) {
      garage(floor - lowestFloor) -= 1
      true
    } else false
  }

  override def toString: String = {
    garage.zipWithIndex.foldLeft(new StringBuilder("Current parking lot status:\n"))(
      (builder, floorTuple) =>
        builder.append(s"Floor: ${floorTuple._2 + lowestFloor}\tfree space: ${floorTuple._1}\n"))
      .result()
  }
}

object ParkingLot {
  def apply(floorRange: (Int, Int), floorCapacity: Int): ParkingLot = {
    val garage = Array.fill[Int](Math.abs(floorRange._2 - floorRange._1) + 1)(floorCapacity)
    val lowestFloor = if (floorRange._2 > floorRange._1) floorRange._1 else floorRange._2
    new ParkingLot(garage, lowestFloor)
  }

  def fromXml(fileName: String): ParkingLot = {
    val xml = XML.loadFile(fileName)
    val lowestFloor = (xml \@ "lowestFloor").toInt
    val array: Array[Int] = (xml \\ "floor")
      .map[Int](node => (node \@ "space").toInt)
      .toArray
    new ParkingLot(array, lowestFloor)
  }
}
