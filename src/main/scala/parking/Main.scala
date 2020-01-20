package parking

import java.io.File

import parking.parkingLot.ParkingLot
import parking.vehicle.VehicleInfo

import scala.io.StdIn
import scala.util.Try

/**
 * Usage 1: ProgramName
 * Usage 2: ProgramName args
 * args: "Vehicle register file name" "Starting lot file name"
 * Usage 3: ProgramName args
 * args: "Vehicle register file name" "number of lowest floor" "number of highest floor" "floor capacity"
 */
object Main extends App {
  // Picking file names from args
  val useDefault = args.length == 0
  val vehicleRegisterFileName: String =
    if (useDefault) s".${File.separator}VehicleRegister.xml"
    else args(0)

  val startingLotFileName: String =
    if (useDefault) s".${File.separator}StartingLot.xml"
    else if (args.length == 2) args(1)
    else ""

  // Parking lot parsing & validation
  val parkingLot: ParkingLot = args.length match {
    case 0 | 2 => loadParkingLotFromXml
    case 4 => createParkingLotFromArgs
    case _ => citeUsageAndExit
  }
  println(parkingLot)

  // Validate vehicle register file
  if (!isFileReadable(vehicleRegisterFileName)) {
    Console.err.println("Fatal: unable to read vehicle register file.")
    sys.exit(-1)
  }

  // Main program loop
  while (true) {
    val input: Either[String, (Int, VehicleInfo)] =
      readInput(vehicleRegisterFileName, parkingLot.isValidFloor)
    val message: String = input match {
      // Left is used for passing all besides main workflow usages
      case Left(infoMessage) => infoMessage
      // Right is used for passing parsed input data to pass to suggestion algorithm
      case Right(data) =>
        val (entryFloor, vehicleInfo) = data
        val predicate = VehicleInfo.defaultVehicleFloorRestrictions(vehicleInfo)
        parkingLot.nearestFloor(entryFloor, predicate) match {
          case Some(value) =>
            val distFromEntry = Math.abs(value - data._1)
            s"Nearest found floor: $value ${
              if (distFromEntry == 0) "(same floor as entry)."
              else s"($distFromEntry floors away from entry)."
            }"
          case None => "No free space found."
        }
    }
    println(message)
  }


  def readInput(fileName: String, validFloorCondition: Int => Boolean): Either[String, (Int, VehicleInfo)] = {
    print("Enter command (help, etc... preceded with \'/\') or vehicle entry floor (integer): ")

    val command = StdIn.readLine().replaceAll("\\s+", "").toLowerCase()
    if (command.length == 0 || command.charAt(0) == '/') {
      command match {
        case "/exit" => sys.exit(0)
        case "/help" => Left("Current commands: /help /exit /example /park /parkForce")
        case "" | "/example" => Left("Input example: enter 0 then enter \"ABC123\".")
        case "/parkforce" => Left(parkForce())
        case "/park" => Left(park(fileName, validFloorCondition))

        case _ => Left("Unrecognized command.")
      }
    } else {
      finishReadingDetails(command, fileName, validFloorCondition)
    }
  }

  def finishReadingDetails
  (
    floorString: String,
    fileName: String,
    validFloorCondition: Int => Boolean
  ): Either[String, (Int, VehicleInfo)] = {
    try {
      val entryFloor = floorString.toInt
      if (!validFloorCondition(entryFloor)) Left("Invalid entry floor.")
      else {
        print("Enter vehicle licence plate (i.e. \"ABC123\"): ")
        val licencePlate = StdIn.readLine().replaceAll("\\s+", "")
        val vehicleInfoOpt = VehicleInfo.fetchInfoXML(fileName, licencePlate)
        vehicleInfoOpt match {
          case None => Left("Licence plate not in registry or wrong vehicle information.")
          case Some(vehicleInfo) => Right((entryFloor, vehicleInfo))
        }
      }
    } catch {
      case _: NumberFormatException => Left("Invalid integer.")
    }
  }


  def parkForce(): String = {
    print("Enter floor to force park a vehicle at: ")
    val floorStr = StdIn.readLine().replaceAll("\\s+", "")
    try {
      val floor = floorStr.toInt
      if (parkingLot.isValidFloor(floor) && parkingLot.floorHasSpace(floor)) {
        parkingLot.parkAtFloor(floor)
        parkingLot.toString()
      } else "Invalid floor"
    } catch {
      case _: NumberFormatException => "Invalid integer"
    }
  }

  def park(fileName: String, validFloorCondition: Int => Boolean): String = {
    print("Enter vehicle entry floor: ")
    val floorStr = StdIn.readLine().replaceAll("\\s+", "")
    finishReadingDetails(floorStr, fileName, validFloorCondition) match {
      case Left(message) => message
      case Right(data) =>
        val (entryFloor, vehicleInfo) = data
        val predicate = VehicleInfo.defaultVehicleFloorRestrictions(vehicleInfo)
        // Find nearest available floor
        parkingLot.nearestFloor(entryFloor, predicate) match {
          case Some(value) =>
            // Reduce empty spaces at found nearest floor
            parkingLot.parkAtFloor(value)
            val distFromEntry = Math.abs(value - data._1)
            s"Vehicle parked at nearest floor found: $value ${
              if (distFromEntry == 0) "(same floor as entry)."
              else s"($distFromEntry floors away from entry)."
            }\n${parkingLot.toString()}"
          case None => "No available space found. Vehicle not parked"
        }
    }
  }


  def isFileReadable(fileName: String): Boolean = {
    new File(fileName).canRead
  }

  def loadParkingLotFromXml: ParkingLot = {
    if (isFileReadable(startingLotFileName)) {
      println("Loading parking lot from file...")
      ParkingLot.fromXml(startingLotFileName)
    } else {
      Console.err.println("Fatal: unable to read parking lot file.")
      sys.exit(-1)
    } // Initialize from xml file
  }

  def createParkingLotFromArgs: ParkingLot = {
    val lowestFloor = args(1)
    val highestFloor = args(2)
    val floorCapacity = args(3)

    val floorRangeOpt = Try {
      (lowestFloor.toInt, highestFloor.toInt)
    }.toOption
    val floorCapacityOpt = Try {
      floorCapacity.toInt
    }.toOption

    (floorRangeOpt, floorCapacityOpt) match {
      // If both of them exist, create a parking lot
      case (Some(floorRange), Some(floorCapacity)) => ParkingLot(floorRange, floorCapacity)
      // If either does not exist
      case _ =>
        println("Unable to parse arguments.")
        sys.exit(1)
    }
  }

  def citeUsageAndExit: Nothing = {
    Console.err.println(
      """
        |Usage 1: ProgramName
        |Usage 2: ProgramName args
        | args: "Vehicle register file name" "Starting lot file name"
        |Usage 3: ProgramName args
        | args: "Vehicle register file name" "number of lowest floor" "number of highest floor" "floor capacity"
        |""".stripMargin)
    sys.exit(1)
  }
}
