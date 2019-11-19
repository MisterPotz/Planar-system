package planar_structure.mechanism


import planar_structure.mechanism.types.{CarrierPosition, CodeGenerator, MechanismType}

import scala.collection.mutable.ListBuffer

//interface of gearstructurecharacteristic
//operates on a given gearstructurestore
trait GearStructureCharacteristic{
  val storage : GearStructureStore
  def toStringShort : String = s"Mechanism Type: "
  def toStringFull : String
  def mechanismTypeString : String
  def setSatellitesAmount(k : Byte) : Unit
  protected def gear_groups : List[GearGroup]
  protected def gear_connections : List[GearConnection]
  def getCode : String
  def getBiggestGear : GearWheel
  def getMaxStageSize : Float
  def getMaxRw : Float
  def getGearList : List[GearWheel]
  def getGearConnectionList : List[GearConnection]
  def getGearGroups : List[GearGroup]
  def getSatelliteGears : List[GearWheel]
  protected def makeGearConnectionList(b : List[(GearWheel, GearWheel)]): List[GearConnection]
  protected def preparePairsForConnections(gear_list : List[GearWheel]) : List[(GearWheel, GearWheel)]
  protected def mkStore(gears : List[GearWheel], mechanismType: MechanismType,
                        carrierPosition: CarrierPosition) : GearStructureStore
}

abstract class GearStructureCharacteristic2KH(gears : List[GearWheel],
                                     mechanismType: MechanismType,
                                     carrierPosition: CarrierPosition
                                    )
  extends GearStructureCharacteristic {
  override val storage: GearStructureStore = mkStore(
    gears, mechanismType, carrierPosition
  )

  override def toStringShort: String = s"Mechanism Type: "

  override def toStringFull: String = "Gear Characteristics"

  override def mechanismTypeString: String = storage.mechanismType.toString

  override def setSatellitesAmount(k: Byte): Unit = storage.k = k

  protected def gear_groups: List[GearGroup] = storage.gearGroups

  protected def gear_connections: List[GearConnection] = storage.gearConnections

  def getCode: String = CodeGenerator(storage.mechanismType, storage.carrierPosition)

  def getBiggestGear: GearWheel = {
    getGearList.maxBy(_.holder.z) //поиск максимальной шестерни по числу зубьев
  }

  override def getMaxRw: Float = {
    getGearConnectionList.maxBy(_.connectionCalculationBehavior.maxRw).connectionCalculationBehavior.maxRw
  }

  override def getMaxStageSize: Float = {
    storage.gearConnections.maxBy(_.connectionCalculationBehavior.stageSize).connectionCalculationBehavior.stageSize
  }
  override def getGearList: List[GearWheel] = storage.gears

  override def getGearConnectionList: List[GearConnection] = storage.gearConnections

  override def getGearGroups: List[GearGroup] = gear_groups

  override def getSatelliteGears: List[GearWheel] //must be overridden in subclasses

  protected def makeGearConnectionList(b: List[(GearWheel, GearWheel)]): List[GearConnection] = b.map { a =>
    new GearConnection(a._1, a._2)
  }

  //really works only for the one-row mechanisms
  protected def preparePairsForConnections(gear_list: List[GearWheel]): List[(GearWheel, GearWheel)]

  /*  @scala.annotation.tailrec
    def recursivePair(list : ListBuffer[(GearWheel, GearWheel)], analyzed_list : List[GearWheel]){
      if (analyzed_list.isEmpty || analyzed_list.tail.isEmpty){
      }
      else {
        list.append((analyzed_list.head, analyzed_list.tail.head))
        recursivePair(list, analyzed_list.tail)
      }
    }
    val listBuffer  = ListBuffer.empty[(GearWheel, GearWheel)]
    recursivePair(listBuffer, gear_list)
    listBuffer.toList*/
  protected def mkStore(gears: List[GearWheel], mechanismType: MechanismType,
                        carrierPosition: CarrierPosition): GearStructureStore = {
    val gearConnections = makeGearConnectionList(preparePairsForConnections(gears))
    val gearGroups = GearGroup(gears, gearConnections)
    new GearStructureStore(GearStructureStoreImmutable(gears,
      gearConnections, gearGroups, mechanismType, carrierPosition),
      new GearStructureStoreMutable)
  }
}