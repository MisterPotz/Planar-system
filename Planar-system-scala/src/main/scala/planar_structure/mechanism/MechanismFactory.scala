package planar_structure.mechanism

import planar_structure.mechanism.types.{CarrierPosition, MechanismType}

trait MechanismFactory{
  def apply(code : String) : Mechanism
  def safeApply(code : String) : Either[Boolean, Mechanism]
  def apply(mechanismType: MechanismType,
            carrierPosition: CarrierPosition,
            z_list: List[Short],
            k : Byte) : Mechanism
}