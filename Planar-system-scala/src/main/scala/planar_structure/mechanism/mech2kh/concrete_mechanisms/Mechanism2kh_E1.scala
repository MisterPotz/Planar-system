package planar_structure.mechanism.mech2kh.concrete_mechanisms

import planar_structure.mechanism.common_mechanisms.Common.WheelCalculator
import planar_structure.mechanism.common_mechanisms.partial_calculators.{CarrierDelegate, PartialWheelCalculator}
import planar_structure.mechanism.{GearStructureCharacteristic, GeometricMethod}
import planar_structure.mechanism.mech2kh.{GearStructure2KHCharacteristic, Geometric2KHMethod, Mechanism2KH}
import planar_structure.mechanism.types.{CarrierPosition, External1}

class Mechanism2kh_E1(carrierPosition: CarrierPosition) extends Mechanism2KH {
  // Geometric2KHMethod(characteristics.gearStructureCharacteristic)\
  override val gearStructureCharacteristic: GearStructureCharacteristic = GearStructure2KHCharacteristic.apply(External1,
    carrierPosition)
  //override lazy val methods: GeometricMethod = Geometric2KHMethod(this)
  calculator = PartialWheelCalculator(External1, carrierPosition) //TODO wheel calculator
}