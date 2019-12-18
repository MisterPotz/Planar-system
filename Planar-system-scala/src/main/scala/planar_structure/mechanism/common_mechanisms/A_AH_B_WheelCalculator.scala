package planar_structure.mechanism.common_mechanisms

import planar_structure.mechanism.common_mechanisms.A_AB_H_WheelCalculator.uCheck
import planar_structure.mechanism.common_mechanisms.B_AH_B_WheelCalculator.canHaveSatellites
import planar_structure.mechanism.process.report.SynthesizedMechanisms

import scala.collection.mutable.ListBuffer

object A_AH_B_WheelCalculator extends WheelCalculator {
  override def U_direct_H(targetU: Float): Float = {
    1 - targetU
  }

  override def findUdH(listBuffer: ListBuffer[Int]): Double = {
    -listBuffer(2) / listBuffer(0)
  }

  override def assemblyCheck(wheelNumbers: ListBuffer[Int], satellites: Int): Boolean = {
    if (canHaveSatellites(wheelNumbers(0), satellites) && canHaveSatellites(wheelNumbers(2), satellites)) true else false
  }

  override def unaccurateAlignment(wheelNumbers: ListBuffer[Int], gears_accuracy: Int = 1): Boolean = {
    if (math.abs(wheelNumbers(0) + wheelNumbers(1) - wheelNumbers(2)) <= gears_accuracy) true else false
  }

  override def neighborhoodCheck(wheelNumbers: ListBuffer[Int], satellites: Int): Boolean = {
    if ((wheelNumbers(0) + wheelNumbers(1)) * math.sin(math.Pi / satellites) > wheelNumbers(1) + 2) true else false
  }

  override def findInitialVariants(targetU: Double, accuracyU: Double, satellites: Int, gear_accuracy: Int = 2): ListBuffer[ListBuffer[Int]] = {
    val za = Range(17, 100, 1).filter(canHaveSatellites(_, satellites))
    val zb = Range(85, 230, 1).filter(canHaveSatellites(_, satellites))
    val final_list = ListBuffer.empty[ListBuffer[Int]]
    //val zg = Range(30, 230, 1)
    for (az <- za) {
      for (bz <- zb) {
        val g = math.round((bz - az) / 2).toInt
        if (g > 17) {
          val list = ListBuffer(az, g, bz)
          if (unaccurateAlignment(list, gear_accuracy)) {
            if (assemblyCheck(list, satellites)) {
              if (neighborhoodCheck(list, satellites)) {
                if (uCheck(list,targetU, accuracyU))
                  final_list.addOne(list)
              }
            }
          }
        }
      }
    }
    final_list
  }

  override def accurateAlignment(z: IndexedSeq[Int])(alpha_t: Double): List[A_AH_B_WheelCalculator.ShiftedWheel] = null //TODO

  override def findFinalVariants(initial_variants: ListBuffer[Int]): ListBuffer[A_AH_B_WheelCalculator.WithShiftedWheels] = null //TODO

  override def findTargetU(u_directH: Double): Double = {
    1 - u_directH
  }

  override def carrierFrequency(inputFreq: Double, wheelList: ListBuffer[Int], kpd: Double): Double = 0
}