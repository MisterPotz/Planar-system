package planar_structure.mechanism

import planar_structure.mechanism.mech2kh.{ExternalExternal, ExternalInternal, GearStructure2KHCharacteristic, InternalExternal, Mechanism2KH}
import planar_structure.mechanism.GearWheel

import scala.collection.mutable.ListBuffer

package object mech2kh_EE {
  class GearStructure2Characteristic
  class Mechanism2kh_EE(carrierPosition: CarrierPosition) extends Mechanism2KH{
    override val methods: MechanismMethodsCase = new MechanismMethodsCase {
      override val geometricMethods: GeometricMethod = null //TODO this one ASAP!
      override val kinematicMethods: KinematicMethod = null
      override val materialStrengthMethods: MaterialStrengthMethod = null
    }
    override val characteristics: MechanismCharacteristicsCase = new MechanismCharacteristicsCase {
      override val inputPhysicalParamsCharacteristic: InputPhysicalParamsCharacteristic = null //TODO physical properties into input
      override val gearStructureCharacteristic: GearStructureCharacteristic = GearStructure2KHCharacteristic(ExternalExternal, carrierPosition)
    }
    override val functionalityUnit: MechanismFunctionality = null //TODO WHY THE HELL DO WE NEED THIS ONE!?
  }
}


package object mech2kh_EI {
  class Mechanism2kh_EI(carrierPosition: CarrierPosition) extends Mechanism2KH{
    override val methods: MechanismMethodsCase = new MechanismMethodsCase {
      override val geometricMethods: GeometricMethod = null //TODO this one ASAP!
      override val kinematicMethods: KinematicMethod = null
      override val materialStrengthMethods: MaterialStrengthMethod = null
    }
    override val characteristics: MechanismCharacteristicsCase = new MechanismCharacteristicsCase {
      override val inputPhysicalParamsCharacteristic: InputPhysicalParamsCharacteristic = null //TODO physical properties into input
      override val gearStructureCharacteristic: GearStructureCharacteristic = GearStructure2KHCharacteristic(ExternalInternal,carrierPosition)
    }
    override val functionalityUnit: MechanismFunctionality = null //TODO WHY THE HELL DO WE NEED THIS ONE!?
  }
}

package object mecha2kh_IE{
  class Mechanism2kh_EI(carrierPosition: CarrierPosition) extends Mechanism2KH{
    override val methods: MechanismMethodsCase = new MechanismMethodsCase {
      override val geometricMethods: GeometricMethod = null //TODO this one ASAP!
      override val kinematicMethods: KinematicMethod = null
      override val materialStrengthMethods: MaterialStrengthMethod = null
    }
    override val characteristics: MechanismCharacteristicsCase = new MechanismCharacteristicsCase {
      override val inputPhysicalParamsCharacteristic: InputPhysicalParamsCharacteristic = null //TODO physical properties into input
      override val gearStructureCharacteristic: GearStructureCharacteristic = GearStructure2KHCharacteristic(InternalExternal,carrierPosition)
    }
    override val functionalityUnit: MechanismFunctionality = null //TODO WHY THE HELL DO WE NEED THIS ONE!?
  }
}
