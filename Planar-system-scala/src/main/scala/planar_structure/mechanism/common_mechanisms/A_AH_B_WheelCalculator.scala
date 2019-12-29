package planar_structure.mechanism.common_mechanisms

import planar_structure.mechanism.common_mechanisms.Common.WheelCalculator
import planar_structure.mechanism.process.report.SynthesizedMechanisms
import planar_structure.mechanism.types.{CarrierOutput, CarrierPosition, External1, MechanismType}

import scala.collection.mutable.ListBuffer

object A_AH_B_WheelCalculator extends A_WheelCalculator {
  override def carrierFrequency(inputFreq: Double, wheelList: List[Int], kpd: Double): Double = {
    inputFreq / findTargetU(findUdH(wheelList))
  }

  override def U_direct_H(targetU: Float): Float = 1 - targetU

  override def findTargetU(u_directH: Double): Double = 1 - u_directH

  override val mechanismType: MechanismType = External1
  override val carrierPosition: CarrierPosition = CarrierOutput
}