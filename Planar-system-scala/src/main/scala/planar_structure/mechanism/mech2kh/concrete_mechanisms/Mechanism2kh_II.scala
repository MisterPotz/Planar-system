package planar_structure.mechanism.mech2kh.concrete_mechanisms

import planar_structure.mechanism.{GearStructureCharacteristic, GeometricMethod, InputPhysicalParamsCharacteristic, KinematicMethod, MaterialStrengthMethod, MechanismCharacteristicsCase, MechanismMethodsCase}
import planar_structure.mechanism.mech2kh.{GearStructure2KHCharacteristic, Geometric2KHMethod, Mechanism2KH}
import planar_structure.mechanism.types.{CarrierPosition, ExternalExternal, InternalInternal}


class Mechanism2kh_II(carrierPosition: CarrierPosition) extends Mechanism2KH{
  override  val characteristics: MechanismCharacteristicsCase = new MechanismCharacteristicsCase(
    null,
    GearStructure2KHCharacteristic(InternalInternal, carrierPosition)
  )
  override val methods: MechanismMethodsCase = new MechanismMethodsCase(
    Geometric2KHMethod(characteristics.gearStructureCharacteristic),
    null,
    null)
}