package planar_structure.mechanism

import planar_structure.mechanism.common_mechanisms.MECHANISM_FULL_CLASSIFIER
import planar_structure.mechanism.types.{CarrierPosition, CodeGenerator, MechanismType}

trait MechanismStructure{
  def getCode : String
  def toStringShort : String = ""
  def toStringFull : String = ""
  def name : String = ""
  def getGears : List[GearWheel]
  def getGearGroups : List[GearGroup]
  def getGearConnections : List[GearConnection]
  def getMechanismType : MechanismType
  def getCarrierPosition : CarrierPosition
  def getStorage : GearStructureStore
}


//----Mechanism------
abstract class Mechanism extends MechanismStructure {
  //Method objects
  //Characteristic objects
  //val fullClassifier : MECHANISM_FULL_CLASSIFIER
  val methods : GeometricMethod
  val gearStructureCharacteristic : GearStructureCharacteristic
  override def getStorage : GearStructureStore = gearStructureCharacteristic.storage
  def getCode : String = gearStructureCharacteristic.getCode
  override def toStringShort : String = ""
  override def toStringFull : String = ""
  override def name : String = ""

  override def getGears : List[GearWheel] = getStorage.gears
  override def getGearGroups : List[GearGroup] = getStorage.gearGroups
  override def getGearConnections : List[GearConnection] = getStorage.gearConnections
  override def getMechanismType : MechanismType = getStorage.mechanismType
  override def getCarrierPosition : CarrierPosition = getStorage.carrierPosition

}

