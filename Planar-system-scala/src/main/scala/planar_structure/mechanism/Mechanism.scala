package planar_structure.mechanism

import planar_structure.mechanism.mech2kh.MechanismType

trait MechanismMethods{

}
//------------Methods of mechanism----------
abstract class MechanismMethodsCase{
  val geometricMethods : GeometricMethod;
  val kinematicMethods : KinematicMethod
  val materialStrengthMethods : MaterialStrengthMethod
}
//------------Characteristics of mechanism----------
abstract class MechanismCharacteristicsCase{
  val inputPhysicalParamsCharacteristic : InputPhysicalParamsCharacteristic
  val gearStructureCharacteristic : GearStructureCharacteristic
}
//Manager that handles passing proper characteristic to proper method
abstract class MechanismFunctionality(_mechanismCharacteristicsCase : MechanismCharacteristicsCase, _mechanismMethodsCase : MechanismMethodsCase){
  val mechanismCharacteristicsCase : MechanismCharacteristicsCase = _mechanismCharacteristicsCase
  val mechanismMethodsCase : MechanismMethodsCase = _mechanismMethodsCase
}

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
