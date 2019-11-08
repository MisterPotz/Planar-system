package planar_structure.mechanism

import planar_structure.mechanism.types.{CarrierPosition, MechanismType}

trait MechanismMethods{

}
//------------Methods of mechanism----------
class MechanismMethodsCase(val geometricMethods : GeometricMethod,
                                     val kinematicMethods : KinematicMethod,
                                    val materialStrengthMethods : MaterialStrengthMethod)

//------------Characteristics of mechanism----------
class MechanismCharacteristicsCase(val inputPhysicalParamsCharacteristic : InputPhysicalParamsCharacteristic,
                                              val gearStructureCharacteristic : GearStructureCharacteristic)
//----Mechanism------
abstract class Mechanism {
  //Method objects
  val methods : MechanismMethodsCase
  //Characteristic objects
  val characteristics : MechanismCharacteristicsCase
  def getCode : String = {
    characteristics.gearStructureCharacteristic.getCode
  }
}

case class MechanismEssential(mechanismType: MechanismType, carrierPosition: CarrierPosition, satellitesInfo: SatellitesInfo)
