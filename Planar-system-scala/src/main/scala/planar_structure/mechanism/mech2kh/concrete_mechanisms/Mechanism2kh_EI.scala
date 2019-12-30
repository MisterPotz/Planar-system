package planar_structure.mechanism.mech2kh.concrete_mechanisms

import planar_structure.mechanism.common_mechanisms.B_AH_B_WheelCalculator
import planar_structure.mechanism.common_mechanisms.Common.WheelCalculator
import planar_structure.mechanism.common_mechanisms.partial_calculators.PartialWheelCalculator
import planar_structure.mechanism.{GearStructureCharacteristic, GeometricMethod, KinematicMethod, MaterialStrengthMethod}
import planar_structure.mechanism.mech2kh.{GearStructure2KHCharacteristic, Geometric2KHMethod, Mechanism2KH}
import planar_structure.mechanism.types.{CarrierPosition, External1, ExternalInternal}

class Mechanism2kh_EI(carrierPosition: CarrierPosition) extends Mechanism2KH{
  //override lazy val methods: GeometricMethod = Geometric2KHMethod(this)
  override val gearStructureCharacteristic = GearStructure2KHCharacteristic(ExternalInternal,carrierPosition)
  calculator = PartialWheelCalculator(ExternalInternal, carrierPosition)
}
