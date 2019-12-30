package planar_structure.mechanism.common_mechanisms

import planar_structure.mechanism.common_mechanisms.A_AB_H_WheelCalculator.canHaveSatellites
import planar_structure.mechanism.common_mechanisms.Common.{CarrierDependent, WheelCalculator}
import planar_structure.mechanism.types.{CarrierInput, CarrierPosition, External1, MechanismType}

import scala.collection.mutable.ListBuffer

//от колеса внутреннего к водилу
object A_HB_A_WheelCalculator extends A_WheelCalculator {
  override def carrierPosition: CarrierPosition = CarrierInput

  override def carrierFrequency(inputFreq: Double, wheelList: List[Int], kpd: Double): Double = {
    val p = math.abs(findUdH(wheelList))
    inputFreq / ((1 + p) / p)
  }

  override def U_direct_H(targetU: Double): Double = 1 / (targetU - 1)

  override def findTargetU(u_directH: Double): Double = (1 + math.abs(u_directH)) / math.abs(u_directH)

  override val carrierDelegate: CarrierDependent = null

  override def maxSupposedZSum(z: List[Int]): Double = z_sum2(z)

  override def maxSupposedSatelliteGear(z: List[Int]): Double = z(1)
}
