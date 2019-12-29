package planar_structure.mechanism.mech2kh.methods

import planar_structure.mechanism.process.report.FullConditionCheck
import planar_structure.mechanism.{GearConnection, GearStructureCharacteristic, GearWheel, GeometricMethod}

//these methods are common for every type of mechanism
class GeometricCommonMethods(meth : GearStructureCharacteristic) extends GeometricMethod{
  override def getGearRatioCarrierStopped: Double = {
   // println("Common GearRatioCarrierStopped")
    structure.getGearConnectionList.foldLeft(1.0)((f, s) => {val res = f * s.U; //println(s"latest U: ${res}");
      res}) //просто считает как обычные зацепления весь механизм с остановленным водилом
  } //from a to the last one with h as stopped

  /*override def assemblyCondition: Boolean = {
    val z = structure.getGearList(0).holder.z.toFloat
    val U1H = U1h_4
    //println(s"Gear ratio in assembly condition: ${U1H}")
    val k = structure.storage.k.toFloat
    @scala.annotation.tailrec
    def calc(p : Int): Boolean ={
      if (p == 50) {
        false
      } else{
        val res : Float = ( z.toFloat / k.toFloat * (1 + p * k)).toFloat
        if (math.abs(res - res.round) == 0){
          true
        }else{
          calc(p+1)
        }
      }
    }
    calc(1)
  }*/
  override var structure: GearStructureCharacteristic = meth
  override def getGearRatio: Double = outerMethod.getGearRatio  //carrier decorated
  override def getGearRatioBackwards: Double = outerMethod.getGearRatioBackwards  //carrier decorated

  /*override def neighborhoodCondition: Boolean = {
    /*//first, we must get the biggest gear on our satellite
    val max_wheel : GearWheel = structure.getSatelliteGears.maxBy(_.holder.da)
    //now we must get the connection where our max_wheel is located
    val connection : GearConnection = structure.getGearConnectionList.find(connection =>
      if (connection.gear1 == max_wheel || connection.gear2 == max_wheel) true else false).get
    //after we got our connection we must calculate the distance between the axes of the satellites
    val distance_between_centers = connection.connectionCalculationBehavior.aw * 2.0f * math.sin(math.Pi / structure.storage.k.toFloat).toFloat
    val difference = distance_between_centers - max_wheel.holder.da
    if (difference >= max_wheel.holder.z * 0.5f){
      true
    } else {
      false
    }*/
    val maxWheelPair = structure.getGearGroups
    maxWheelPair.length match {
      case 1 =>
        //если группа одна, то макс размер сателлита - посередке
        val z_min = math.min(maxWheelPair(0).gear_list(0).holder.z, maxWheelPair(0).gear_list(2).holder.z)
        (z_min * math.sin(math.Pi / structure.get) > wheelNumbers(1) + 2)
      case 2 =>
        //две группы зацеплений
    }
  }*/

  override def U14_h: Double = getGearRatioCarrierStopped
}