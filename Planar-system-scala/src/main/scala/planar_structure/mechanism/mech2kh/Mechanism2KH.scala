package planar_structure.mechanism.mech2kh

import planar_structure.mechanism.{CarrierOutput, CarrierPosition, CarrierPositionInfo, ExternalWheel, GearConnection, GearGeometricCharacteristic, GearStructureCharacteristic, GearWheel, GeometricMethod, InternalWheel, MaterialCharacteristic, Mechanism, MechanismCharacteristicsCase, MechanismFunctionality, MechanismMethodsCase, Methodable}

import scala.collection.mutable.ListBuffer


abstract class Mechanism2KH extends Mechanism

//used to understand what wheels are present in the mechanism
sealed trait MechanismType;
case object ExternalInternal extends MechanismType
case object InternalExternal extends MechanismType
case object ExternalExternal extends MechanismType


//constructor for proper gear structure characteristics
object GearStructure2KHCharacteristic{
  trait GearStructure2KH_4Wheels_Characteristic extends GearStructureCharacteristic{
    override def getSatelliteGears: List[GearWheel] = getGearList.slice(1,3)
  }
  trait GearStructure2KH_3Wheels_Characteristis extends GearStructureCharacteristic{
    override def getSatelliteGears: List[GearWheel] = getGearList.slice(1,2)
  }
  def apply(mechanismType: MechanismType, carrierPosition: CarrierPosition = CarrierOutput): GearStructureCharacteristic ={
    mechanismType match {
      case ExternalExternal => new GearStructure2KH_4Wheels_Characteristic {
        override protected val gears: List[GearWheel] = (for (i <- Range(0,4)) yield new ExternalWheel()).toList
        override val info: CarrierPosition = carrierPosition

      }
      case InternalExternal => new GearStructure2KH_4Wheels_Characteristic {
        override protected val gears: List[GearWheel] = new InternalWheel() :: new ExternalWheel() :: new ExternalWheel() :: new  ExternalWheel() :: Nil
        override val info: CarrierPosition = carrierPosition
      }
      case ExternalInternal => new GearStructure2KH_4Wheels_Characteristic {
        override protected val gears: List[GearWheel] = new ExternalWheel() :: new ExternalWheel() :: new ExternalWheel() :: new  InternalWheel() :: Nil
        override val info: CarrierPosition = carrierPosition
      }
    }
  }
}
object Geometric2KHMethod{
  def apply(gearGeometricCharacteristic: GearStructureCharacteristic): GeometricMethod = new GeometricMethod {
    override var methodable: Methodable = gearGeometricCharacteristic

  }

}