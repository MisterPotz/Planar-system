package planar_structure.mechanism.common_mechanisms

import planar_structure.mechanism.common_mechanisms.A_AH_B_WheelCalculator.{assemblyCheck, canHaveSatellites, neighborhoodCheck, unaccurateAlignment}
import planar_structure.mechanism.common_mechanisms.Common.WheelCalculator
import planar_structure.mechanism.types.{CarrierNeutral, CarrierPosition, External1, MechanismType}

import scala.collection.mutable.ListBuffer

object A_AB_H_WheelCalculator extends A_WheelCalculator {
  override def carrierFrequency(inputFreq: Double, wheelList: List[Int], kpd: Double): Double = {
    0
  }

  override def U_direct_H(targetU: Float): Float = targetU

  override def findTargetU(u_directH: Double): Double = u_directH

  override val mechanismType: MechanismType = External1
  override val carrierPosition: CarrierPosition = CarrierNeutral
}
