package planar_structure.mechanism.mech2kh.concrete_mechanisms

import planar_structure.mechanism.common_mechanisms.C_HB_E_WheelCalculator
import planar_structure.mechanism.common_mechanisms.Common.WheelCalculator
import planar_structure.mechanism.common_mechanisms.partial_calculators.PartialWheelCalculator
import planar_structure.mechanism.{GearStructureCharacteristic, GeometricMethod, KinematicMethod, MaterialStrengthMethod}
import planar_structure.mechanism.mech2kh.{GearStructure2KHCharacteristic, Geometric2KHMethod, Mechanism2KH}
import planar_structure.mechanism.types.{CarrierPosition, External1, ExternalExternal, InternalInternal}


class Mechanism2kh_II(carrierPosition: CarrierPosition) extends Mechanism2KH{
  override val gearStructureCharacteristic = GearStructure2KHCharacteristic(InternalInternal, carrierPosition)
  //override lazy val methods: GeometricMethod = Geometric2KHMethod(this)
  calculator  = PartialWheelCalculator(InternalInternal, carrierPosition)

}