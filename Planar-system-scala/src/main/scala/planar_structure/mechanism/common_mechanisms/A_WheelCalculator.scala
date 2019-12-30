package planar_structure.mechanism.common_mechanisms

import planar_structure.mechanism.common_mechanisms.Common.WheelCalculator
import planar_structure.mechanism.types.{External1, MechanismType}

import scala.collection.mutable.ListBuffer

//первое колесо всегд внешнее, от внешнего к внутреннему
trait A_WheelCalculator extends WheelCalculator {
  override val mechanismType: MechanismType = External1
  override def sign: Double = -1

  override def findUdH(listBuffer: List[Int]) : Double = {
    listBuffer.length match {
      case 4 => sign * listBuffer(1) / listBuffer(0).toFloat * listBuffer(3) / listBuffer(2).toFloat
      case 3 => sign * listBuffer(2) / listBuffer(0)
    }
  }

  override def assemblyCheck(wheelNumbers: List[Int], satellites: Int): Boolean = {
    if (canHaveSatellites(wheelNumbers(0), satellites) && canHaveSatellites(wheelNumbers(2), satellites)) true else false
  }

  override def unaccurateAlignment(wheelNumbers: List[Int], gears_accuracy: Int): Boolean = {
    if (math.abs(wheelNumbers(0) + wheelNumbers(1) + wheelNumbers(1) - wheelNumbers(2)) <= gears_accuracy) true else false
  }

  override def neighborhoodCheck(wheelNumbers: List[Int], satellites: Int): Boolean = {
    if ((wheelNumbers(0) + wheelNumbers(1)) * math.sin(math.Pi / satellites) > wheelNumbers(1) + 2) true else false
  }

  override def findInitialVariants(targetU: Double, accuracyU: Double, satellites: Int, gear_accuracy: Int): ListBuffer[List[Int]] ={
    //gear accuracy in this type of mechanism must be smaller
    val za = Range(12, 120, 1).filter(canHaveSatellites(_, satellites))
    val zb = Range(85, 230, 1).filter(canHaveSatellites(_, satellites))
    val final_list = ListBuffer.empty[List[Int]]
    //val zg = Range(30, 230, 1)
    for (az <- za) {
      for (bz <- zb) {
        val g = math.round((bz - az) / 2.toFloat)
        if (g > 12) {
          val list = List(az, g, bz)
          if (unaccurateAlignment(list, 1)) {
            if (assemblyCheck(list, satellites)) {
              if (neighborhoodCheck(list, satellites)) {
                if (uCheckBooleanRaw(list, targetU, accuracyU))
                  final_list.addOne(list)
              }
            }
          }
        }
      }
    }
    final_list
  }

  override def z_sum1(z: List[Int]): Int = z(0) + z(1)

  override def totalShift1(x: List[Double]): Double = x(0)+x(1)

  override def z_sum2(z: List[Int]): Int = z(2)-z(1)

  override def totalShift2(x: List[Double]): Double = x(2)-x(1)

  override def getInners: List[Boolean] = List(false,false,true)

  override def getTargetRights: List[Boolean] = List(true, true)
}
