package planar_structure.mechanism

import scala.collection.mutable.ListBuffer

abstract class Characteristic

trait GearStructureStore{
  this : GearStructureCharacteristic =>
  protected val gears : List[GearWheel]
  protected val gear_connections : List[GearConnection] = makeGearConnectionList(preparePairsForConnections)
}
//position of carrier is important and is used to calculate proper gear ratio and understand structure of mechanism
sealed trait CarrierPosition
case object CarrierInput extends CarrierPosition
case object CarrierOutput extends CarrierPosition
case object CarrierNeutral extends CarrierPosition

trait CarrierPositionInfo{
  val info : CarrierPosition
}

trait GearStructureCharacteristic extends Characteristic with GearStructureStore with CarrierPositionInfo with GeometricMethodable{
  def getBiggestGear : GearWheel // TODO  сделать нахождение самой большой шестерни
  def getMaxRw : Double //TODO сделать нахождение максимального размера
  def getGearList : List[GearWheel] = gears
  def getGearConnectionList : List[GearConnection] = gear_connections
  def getGear(i : Int) : GearWheel = getGearList(i)
  def getConnection(i : Int) : GearConnection =  getGearConnectionList(i)
  def getSatelliteGears : List[GearWheel]
  def makeGearConnectionList(b : List[(GearWheel, GearWheel)]): List[GearConnection] = b.map{ a =>
    new GearConnection(a._1, a._2)
  }
  def preparePairsForConnections : List[(GearWheel, GearWheel)] = {
    @scala.annotation.tailrec
    def recursivePair(list : ListBuffer[(GearWheel, GearWheel)], analyzed_list : List[GearWheel]){
      if (analyzed_list.nonEmpty && analyzed_list.tail.nonEmpty){
        list.append((analyzed_list.head, analyzed_list.tail.head))
      }
      else
        recursivePair(list, analyzed_list.tail)
    }
    val listBuffer  = ListBuffer.empty[(GearWheel, GearWheel)]
    recursivePair(listBuffer, getGearList)
    listBuffer.toList
  }
}

trait GearRelativePositionCharacteristic extends  Characteristic
trait InputPhysicalParamsCharacteristic extends Characteristic



