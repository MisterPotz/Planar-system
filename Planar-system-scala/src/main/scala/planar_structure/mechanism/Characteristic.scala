package planar_structure.mechanism


import planar_structure.mechanism.types.{CarrierPosition, MechanismType}

import scala.collection.mutable.ListBuffer

abstract class Characteristic

//TODO this one was a trait before
class GearStructureStore(
                                   val gears : List[GearWheel],
                                   val gear_connections : List[GearConnection],
                                   val gear_groups : List[GearGroup]
                                 ){
}

trait CarrierPositionInfo{
  val info : CarrierPosition
}
trait SatellitesInfo{
  val amount_of_satellites : Int = 3 //amount of satellites
}


abstract class GearStructureCharacteristic(gears : List[GearWheel],
                                  val mechanismType : MechanismType,
                                  val info : CarrierPosition,
                                 var amount_of_satellites : Int = 3) extends Characteristic{

  val storage : GearStructureStore = {
    val gear_connections_ = makeGearConnectionList(preparePairsForConnections)
    new GearStructureStore(gears,
      gear_connections_,
      GearGroup(gears, gear_connections_))
  }
  def setSatellitesAmount(k : Int) : Unit = amount_of_satellites = k
  protected def gear_groups : List[GearGroup] = storage.gear_groups
  protected def gear_connections : List[GearConnection] = storage.gear_connections
  def setStorage(gears : List[GearWheel]) : Unit = {

  }
  def getCode : String = mechanismType.toCode +  "_"+ info.toCode
  def getBiggestGear : GearWheel = {
    getGearList.maxBy(_.holder.z) //поиск максимальной шестерни по числу зубьев
  }
  def getMaxRw : Double = {
    getGearConnectionList.maxBy(_.connectionCalculationBehavior.maxRw).connectionCalculationBehavior.maxRw
  }
  def getGearList : List[GearWheel] = gears
  def getGearConnectionList : List[GearConnection] = gear_connections
  def getGear(i : Int) : GearWheel = getGearList(i)
  def getConnection(i : Int) : GearConnection =  getGearConnectionList(i)
  def getGearGroups : List[GearGroup] = gear_groups
  def getGearGroup(i : Int) : GearGroup = gear_groups(i)
  def getSatelliteGears : List[GearWheel]
  def makeGearConnectionList(b : List[(GearWheel, GearWheel)]): List[GearConnection] = b.map{ a =>
    new GearConnection(a._1, a._2)
  }
  def preparePairsForConnections : List[(GearWheel, GearWheel)] = {
    @scala.annotation.tailrec
    def recursivePair(list : ListBuffer[(GearWheel, GearWheel)], analyzed_list : List[GearWheel]){
      if (analyzed_list.isEmpty || analyzed_list.tail.isEmpty){
      }
      else {
        list.append((analyzed_list.head, analyzed_list.tail.head))
        recursivePair(list, analyzed_list.tail)
      }

    }
    val listBuffer  = ListBuffer.empty[(GearWheel, GearWheel)]
    recursivePair(listBuffer, getGearList)
    listBuffer.toList
  }
}

trait GearRelativePositionCharacteristic extends  Characteristic
trait InputPhysicalParamsCharacteristic extends Characteristic

