package planar_structure.mechanism.mech2kh.concrete_mechanisms

import planar_structure.mechanism.{GearStructureCharacteristic, GeometricMethod, KinematicMethod, MaterialStrengthMethod}
import planar_structure.mechanism.mech2kh.{GearStructure2KHCharacteristic, Geometric2KHMethod, Mechanism2KH}
import planar_structure.mechanism.types.{CarrierPosition, ExternalExternal}

class Mechanism2kh_EE(carrierPosition: CarrierPosition) extends Mechanism2KH{
  override val gearStructureCharacteristic: GearStructureCharacteristic = GearStructure2KHCharacteristic(ExternalExternal, carrierPosition)
  override lazy val methods: GeometricMethod = Geometric2KHMethod(this)

}