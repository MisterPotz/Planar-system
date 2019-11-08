package planar_structure.mechanism.mech2kh

import planar_structure.mechanism.{Characteristic, ExternalWheel, GearStructureCharacteristic, GearWheel, GeometricMethod, InternalWheel, Mechanism}
import planar_structure.mechanism.types.{External1, Internal1, InternalInternal, _}

import scala.collection.mutable.ListBuffer

//constructor for proper gear structure characteristics
object GearStructure2KHCharacteristic{
  //TODO make decorators for this things
  class GearStructure2KH_4Wheels_Characteristic(gears : List[GearWheel],
                                                mechanismType : MechanismType,
                                                info : CarrierPosition)
    extends GearStructureCharacteristic(gears, mechanismType, info){
    override def getSatelliteGears: List[GearWheel] = getGearList.slice(1,3)
    override def preparePairsForConnections: List[(GearWheel, GearWheel)] = {
      (getGearList(0), getGearList(1)) :: (getGearList(2), getGearList(3)) :: Nil;
    }
  }
  class GearStructure2KH_3Wheels_Characteristis(gears : List[GearWheel],
                                                mechanismType : MechanismType,
                                                info : CarrierPosition)
    extends GearStructureCharacteristic(gears, mechanismType, info){
    override def getSatelliteGears: List[GearWheel] = getGearList.slice(1,2)
    override def preparePairsForConnections: List[(GearWheel, GearWheel)] = {
      (getGearList(0), getGearList(1)) :: (getGearList(1), getGearList(2)) :: Nil;
    }
  }
  def apply(mechanismType_ : MechanismType, carrierPosition: CarrierPosition = CarrierOutput): GearStructureCharacteristic ={
    mechanismType_ match {
      case ExternalExternal => new GearStructure2KH_4Wheels_Characteristic(
        new ExternalWheel() :: new ExternalWheel() :: new ExternalWheel() :: new  ExternalWheel() :: Nil,
        mechanismType_,
        carrierPosition
      )
      case InternalExternal => new GearStructure2KH_4Wheels_Characteristic(
        new InternalWheel() :: new ExternalWheel() :: new ExternalWheel() :: new  ExternalWheel() :: Nil,
        mechanismType_,
        carrierPosition)

      case ExternalInternal => new GearStructure2KH_4Wheels_Characteristic(
        new ExternalWheel() :: new ExternalWheel() :: new ExternalWheel() :: new  InternalWheel() :: Nil,
        mechanismType_,
        carrierPosition)

      case InternalInternal => new GearStructure2KH_4Wheels_Characteristic(
        new InternalWheel() :: new ExternalWheel() :: new ExternalWheel() :: new  InternalWheel() :: Nil,
        mechanismType_,
        carrierPosition)

      case External1 => new GearStructure2KH_3Wheels_Characteristis(
        new ExternalWheel() :: new ExternalWheel() ::  new  InternalWheel() :: Nil,
        mechanismType_,
        carrierPosition)

      case Internal1 => new GearStructure2KH_3Wheels_Characteristis(
          new InternalWheel() :: new ExternalWheel() ::  new  ExternalWheel() :: Nil,
          mechanismType_,
          carrierPosition)
    }
  }
}