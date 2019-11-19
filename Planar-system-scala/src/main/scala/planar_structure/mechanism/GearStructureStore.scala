package planar_structure.mechanism

import planar_structure.mechanism.types.{CarrierPosition, MechanismType}

case class GearStructureStoreImmutable(gears : List[GearWheel],
                                       gear_connections : List[GearConnection],
                                       gear_groups : List[GearGroup],
                                       mechanismType : MechanismType,
                                       carPos : CarrierPosition)

class GearStructureStoreMutable(var amount_of_satellites : Byte = 3)

class GearStructureStore(gearStructureStoreImmutable: GearStructureStoreImmutable,
                         gearStructureStoreMutable: GearStructureStoreMutable){
  def mutable : GearStructureStoreMutable = gearStructureStoreMutable
  def immutable : GearStructureStoreImmutable = gearStructureStoreImmutable
  def gears : List[GearWheel] = immutable.gears
  def gearConnections : List[GearConnection] = immutable.gear_connections
  def gearGroups : List[GearGroup] = immutable.gear_groups
  def mechanismType : MechanismType = immutable.mechanismType
  def carrierPosition : CarrierPosition = immutable.carPos
  def k : Byte = mutable.amount_of_satellites
  def k_$eq(i : Byte): Unit = mutable.amount_of_satellites = i

}