package planar_structure.mechanism.common_mechanisms

import planar_structure.mechanism.common_mechanisms.A_AB_H_WheelCalculator.{canHaveSatellites, uCheck}
import planar_structure.mechanism.common_mechanisms.Common.WheelCalculator
import planar_structure.mechanism.types.{CarrierInput, CarrierPosition, External1, MechanismType}

import scala.collection.mutable.ListBuffer

//от колеса внутреннего к водилу
object A_HB_A_WheelCalculator extends A_WheelCalculator {
  override def carrierFrequency(inputFreq: Double, wheelList: List[Int], kpd: Double): Double = {
    val p = math.abs(findUdH(wheelList))
    inputFreq / ((1 + p) / p)
  }

  override def U_direct_H(targetU: Float): Float = 1 / (targetU - 1)

  override def findTargetU(u_directH: Double): Double = (1 + math.abs(u_directH)) / math.abs(u_directH)

  override val carrierPosition: CarrierPosition = CarrierInput
}
