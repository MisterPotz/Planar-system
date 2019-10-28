package planar_structure.mechanism

import scala.collection.mutable.ListBuffer

abstract class Characteristic

trait GearStructureCharacteristic extends Characteristic{
  def getGearList : List[GearWheel]
  def getGearConnectionList : List[GearConnection]
  def getGear(i : Int) : GearWheel = getGearList(i)
  def getConnection(i : Int) : GearConnection =  getGearConnectionList(i)
  def getSatelliteGears : List[GearWheel]
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



