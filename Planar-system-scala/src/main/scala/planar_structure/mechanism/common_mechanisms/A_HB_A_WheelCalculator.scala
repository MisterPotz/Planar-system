package planar_structure.mechanism.common_mechanisms

import planar_structure.mechanism.common_mechanisms.A_AB_H_WheelCalculator.{canHaveSatellites, uCheck}
import planar_structure.mechanism.common_mechanisms.Common.WheelCalculator

import scala.collection.mutable.ListBuffer
//от колеса внутреннего к водилу
object A_HB_A_WheelCalculator extends WheelCalculator {
    override def U_direct_H(targetU: Float): Float = {
      1 - targetU
    }

    override def findUdH(listBuffer: List[Int]): Double = {
      -listBuffer(2) / listBuffer(0)
    }

    override def assemblyCheck(wheelNumbers: List[Int], satellites: Int): Boolean = {
      if (canHaveSatellites(wheelNumbers(0), satellites) && canHaveSatellites(wheelNumbers(2), satellites)) true else false
    }

    override def unaccurateAlignment(wheelNumbers: List[Int], gears_accuracy: Int = 1): Boolean = {
      if (math.abs(wheelNumbers(0) + wheelNumbers(1) - wheelNumbers(2)) <= gears_accuracy) true else false
    }

    override def neighborhoodCheck(wheelNumbers: List[Int], satellites: Int): Boolean = {
      if ((wheelNumbers(0) + wheelNumbers(1)) * math.sin(math.Pi / satellites) > wheelNumbers(1) + 2) true else false
    }
    override def findInitialVariants(targetU: Double, accuracyU: Double, satellites: Int, gear_accuracy: Int): ListBuffer[List[Int]] = {
      val za = Range(17, 100, 1).filter(canHaveSatellites(_, satellites))
      val zb = Range(85, 230, 1).filter(canHaveSatellites(_, satellites))
      val final_list = ListBuffer.empty[List[Int]]
      //val zg = Range(30, 230, 1)
      for (az <- za) {
        for (bz <- zb) {
          val g = math.round((bz - az) / 2).toInt
          if (g > 17) {
            val list = List(az, g, bz)
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

    override def accurateAlignment(z: IndexedSeq[Int])(alpha_t: Double): List[ShiftedWheel] = null //TODO

    override def findFinalVariants(initial_variants: ListBuffer[Int]): ListBuffer[WithShiftedWheels] = null //TODO

    override def findTargetU(u_directH: Double): Double = {
      (1 + math.abs(u_directH)) / math.abs(u_directH)
    }

  override def carrierFrequency(inputFreq: Double, wheelList: List[Int], kpd: Double): Double = 0

  override def sign: Double = ???

  override def accurateAlignment(z: List[Int], x: List[Double], betas: List[Double], m: List[Double], accuracy: Double): Boolean = ???

  override def z_sum1(z: List[Int]): Int = ???

  override def totalShift1(x: List[Double]): Double = ???

  override def z_sum2(z: List[Int]): Int = ???

  override def totalShift2(x: List[Double]): Double = ???

  override def getInners: List[Boolean] = List(true, false)

  override def getTargetRights: List[Boolean] = List(false, false)
}
