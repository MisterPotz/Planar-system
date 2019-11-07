package planar_structure.mechanism

import planar_structure.mechanism.mech2kh.MechanismType

import scala.collection.mutable.ListBuffer

abstract class Characteristic

trait GearStructureStore{
  this : GearStructureCharacteristic =>
  protected val gears : List[GearWheel]
  protected lazy val gear_connections : List[GearConnection] = makeGearConnectionList(preparePairsForConnections)
  protected lazy val gear_groups : List[GearGroup] = GearGroup(gears, gear_connections)//TODO check geargroup

}
//position of carrier is important and is used to calculate proper gear ratio and understand structure of mechanism
sealed trait CarrierPosition{
  def toCode : String
}
case object CarrierInput extends CarrierPosition {
  override def toCode: String = "CarrierInput"
  override def toString: String = "CarrierInput"
}
case object CarrierOutput extends CarrierPosition{
  override def toCode: String = "CarrierOutput"
  override def toString: String = "CarrierOutput"
}

case object CarrierNeutral extends CarrierPosition{
  override def toCode: String = "CarrierNeutral"
  override def toString: String = "CarrierNeutral"
}

trait CarrierPositionInfo{
  val info : CarrierPosition
}
trait SatellitesInfo{
  val amount_of_satellites : Int = 3 //amount of satellites
}

trait GearStructureCharacteristic extends Characteristic with GearStructureStore with CarrierPositionInfo with SatellitesInfo {
  val mechanismType : MechanismType
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

