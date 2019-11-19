package planar_structure.mechanism.mech2kh

import planar_structure.mechanism.{ExternalWheel, GearStructureCharacteristic, GearStructureCharacteristic2KH, GearWheel, GeometricMethod, InternalWheel, Mechanism, WheelHolder}
import planar_structure.mechanism.types.{External1, Internal1, InternalInternal, _}

import scala.collection.mutable.ListBuffer

//constructor for proper gear structure characteristics
object GearStructure2KHCharacteristic{
  class GearStructure2KH_4Wheels_Characteristic(gears : List[GearWheel],
                                                mechanismType : MechanismType,
                                                info : CarrierPosition)
    extends GearStructureCharacteristic2KH(gears, mechanismType, info){
    override def getSatelliteGears: List[GearWheel] = getGearList.slice(1,3)
    override protected def preparePairsForConnections(gear_list : List[GearWheel]) : List[(GearWheel, GearWheel)] = {
      (gear_list(0), gear_list(1)) :: (gear_list(2), gear_list(3)) :: Nil;
    }
  }
  class GearStructure2KH_3Wheels_Characteristic(gears : List[GearWheel],
                                                mechanismType : MechanismType,
                                                info : CarrierPosition)
    extends GearStructureCharacteristic2KH(gears, mechanismType, info){
    override def getSatelliteGears: List[GearWheel] = getGearList.slice(1,2)
    override def preparePairsForConnections(gear_list : List[GearWheel]) : List[(GearWheel, GearWheel)] = {
      (gear_list(0), gear_list(1)) :: (gear_list(1), gear_list(2)) :: Nil;
    }
  }
  def apply(mechanismType_ : MechanismType, carrierPosition: CarrierPosition = CarrierOutput): GearStructureCharacteristic ={
    mechanismType_ match {
      case ExternalExternal => new GearStructure2KH_4Wheels_Characteristic(
        new ExternalWheel() :: new ExternalWheel(WheelHolder.externalSatellite)
          :: new ExternalWheel(WheelHolder.externalSatellite) :: new  ExternalWheel() :: Nil,
        mechanismType_,
        carrierPosition
      )
      case InternalExternal => new GearStructure2KH_4Wheels_Characteristic(
        new InternalWheel() :: new ExternalWheel(WheelHolder.externalSatellite)
          :: new ExternalWheel(WheelHolder.externalSatellite) :: new  ExternalWheel() :: Nil,
        mechanismType_,
        carrierPosition)

      case ExternalInternal => new GearStructure2KH_4Wheels_Characteristic(
        new ExternalWheel() :: new ExternalWheel(WheelHolder.externalSatellite)
          :: new ExternalWheel(WheelHolder.externalSatellite) :: new  InternalWheel() :: Nil,
        mechanismType_,
        carrierPosition)

      case InternalInternal => new GearStructure2KH_4Wheels_Characteristic(
        new InternalWheel() :: new ExternalWheel(WheelHolder.externalSatellite)
          :: new ExternalWheel(WheelHolder.externalSatellite):: new  InternalWheel() :: Nil,
        mechanismType_,
        carrierPosition)

      case External1 => new GearStructure2KH_3Wheels_Characteristic(
        new ExternalWheel() ::  new ExternalWheel(WheelHolder.externalSatellite)::  new  InternalWheel() :: Nil,
        mechanismType_,
        carrierPosition)

      case Internal1 => new GearStructure2KH_3Wheels_Characteristic(
          new InternalWheel() :: new ExternalWheel(WheelHolder.externalSatellite)::  new  ExternalWheel() :: Nil,
          mechanismType_,
          carrierPosition)
    }
  }
}