package planar_structure.mechanism.common_mechanisms.partial_calculators

import planar_structure.mechanism.common_mechanisms.Common.{CarrierDependent, PartialWheelCalculator}
import planar_structure.mechanism.types.{CarrierNeutral, CarrierPosition}

class CNeutralDelegate(partial : PartialWheelCalculator) extends CarrierDependent(partial){
  override val carrierPosition: CarrierPosition = CarrierNeutral

  /**
   *
   * @param targetU целевое передаточное отношение
   * @return передаточное отношение при остановленном водиле
   */
  override def U_direct_H(targetU: Double): Double = targetU

  /**
   *
   * @param u_directH передаточное отношение от первого звена к последнему при остановленном водиле
   * @return целевое передаточное отношение
   */
  override def findTargetU(u_directH: Double): Double = u_directH
}