package planar_structure.mechanism

import scala.collection.mutable.ListBuffer

case class GearGroupCommonParameters(m : Float, alpha : Float = 20.toRadians, beta : Float = 0)
//GearGroup stores only the gears that are connected directly
class GearGroup(val gear_list : List[GearWheel]) {
  //println("GearGroup created")
  def setCommon(common : GearGroupCommonParameters): GearGroup ={
    gear_list.foreach((gear) => {gear.holder.m = common.m;
      gear.holder.alpha = common.alpha
    gear.holder.beta = common.beta})
    this
  }
  def getCommon : GearGroupCommonParameters = {
    GearGroupCommonParameters(gear_list(0).holder.m, gear_list(0).holder.alpha, gear_list(0).holder.beta)
  }
}
object GearGroup{
  //create lists of intersected gear groups with same characteristics
  def apply(gear_list:List[GearWheel], gearConnection_list : List[GearConnection]) : List[GearGroup] = {
    //this func finds all lists of gears that are connected directly
    @scala.annotation.tailrec
    def getListOfInterconnectedGears(gear : GearWheel = null, gearConnections : List[GearConnection],
                                     list_to_append : ListBuffer[ListBuffer[GearWheel]]) : List[List[GearWheel]] = {
      if (gearConnections.isEmpty){
        list_to_append.toList.map{_.toList}
      }
      else{
       // if (gear eq gearConnections.head.gear1){
         //  list_to_append.last.addOne(gearConnections.head.gear1)
          if(list_to_append.isEmpty){
            list_to_append.addOne(ListBuffer(gearConnections.head.gear1, gearConnections.head.gear2))
          } else if (gear eq gearConnections.head.gear1) {
            list_to_append.last.addOne(gearConnections.head.gear2)
          } else {
            list_to_append.addOne(ListBuffer(gearConnections.head.gear1, gearConnections.head.gear2))
          }
          getListOfInterconnectedGears(gearConnections.head.gear2, gearConnections.tail, list_to_append)
        }
      }
    val lists = getListOfInterconnectedGears(gearConnection_list(0).gear2, gearConnection_list, ListBuffer.empty[ListBuffer[GearWheel]])
    lists.map(new GearGroup(_))
    }
}